{-# LANGUAGE BangPatterns #-}

module Pygmalion.Database.Manager
( runDatabaseManager
, ensureDB
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Data.Hashable (hash)
import Data.List (foldl')
import Data.Time.Clock

import Control.Concurrent.Chan.Len
import Pygmalion.Core
import Pygmalion.Database.IO
import Pygmalion.Database.Request
import Pygmalion.File
import Pygmalion.Index.Stream
import Pygmalion.Log

runDatabaseManager :: DBUpdateChan -> DBQueryChan -> IndexStream -> IO ()
runDatabaseManager updateChan queryChan iStream = do
    start <- getCurrentTime
    withDB $ \h -> do
      let ctx = DBContext h iStream updateChan
      go ctx 0 start
  where
    go :: DBContext -> Int -> UTCTime -> IO ()
    go !ctx 1000 !start = do 
      stop <- getCurrentTime
      logInfo $ "Handled 1000 updates in " ++ show (stop `diffUTCTime` start)
      newStart <- getCurrentTime
      go ctx 0 newStart
    go !ctx !n !s = {-# SCC "databaseThread" #-}
           do !item <- atomically $ readFromChannels updateChan queryChan
              case item of
                Left ups         -> runReaderT (routeUpdates ups) ctx >> go ctx (n+1) s
                Right DBShutdown -> logInfo "Shutting down DB thread"
                Right req        -> runReaderT (route req) ctx >> go ctx (n+1) s
                
readFromChannels :: DBUpdateChan -> DBQueryChan -> STM (Either [DBUpdate] DBRequest)
readFromChannels updateChan queryChan = readQueryChan `orElse` readUpdateChan
  where
    readQueryChan  = Right <$> readTBQueue queryChan
    readUpdateChan = Left <$> readTBQueue updateChan

data DBContext = DBContext
  { dbHandle      :: !DBHandle
  , dbIndexStream :: !IndexStream
  , dbUpdateChan  :: !DBUpdateChan
  }
type DB a = ReaderT DBContext IO a

routeUpdates :: [DBUpdate] -> DB ()
routeUpdates ups = mapM_ routeUpdate ups
  where
    routeUpdate (DBUpdateDef !di)            = update "definition" updateDef di
    routeUpdate (DBUpdateRef !rf)            = update "reference" updateReference rf
    routeUpdate (DBUpdateOverride !ov)       = update "override" updateOverride ov
    routeUpdate (DBUpdateCommandInfo !ci)    = update "command info" updateSourceFile ci
    routeUpdate (DBUpdateFile !sf !t)        = update2 "file" updateFile sf t
    routeUpdate (DBUpdateInclusion !ic)      = update "inclusion" updateInclusion ic
    routeUpdate (DBResetMetadata !sf)        = update "resetted metadata" resetMetadata sf
    
route :: DBRequest -> DB ()
route (DBGetCommandInfo !f !v)                   = getCommandInfoQuery f v
route (DBGetSimilarCommandInfo !f !v)            = getSimilarCommandInfoQuery f v
route (DBGetDefinition !sl !v)                   = query "definition" getDef sl v
route (DBGetInclusions !sf !v)                   = query "inclusions" getInclusions sf v
route (DBGetIncluders !sf !v)                    = query "includers" getIncluders sf v
route (DBGetDirectIncluders !sf !v)              = query "direct includers" getDirectIncluders sf v
--route (DBGetIncluderInfo !sf !v)                 = query "includer info" getIncluderInfo sf v
route (DBGetInclusionHierarchy !sf !v)           = query "inclusion hierarchy"
                                                         getInclusionHierarchy sf v
route (DBGetCallers !sl !v)                      = query "callers" getCallers sl v
route (DBGetCallees !usr !v)                     = query "callees" getCallees usr v
route (DBGetBases !usr !v)                       = query "bases" getOverrided usr v
route (DBGetOverrides !usr !v)                   = query "overrides" getOverriders usr v
route (DBGetMembers !usr !v)                     = query "members" getMembers usr v
route (DBGetRefs !usr !v)                        = query "references" getReferences usr v
route (DBGetReferenced !sl !v)                   = query "referenced" getReferenced sl v
route (DBGetDeclReferenced !sl !v)               = query "decl referenced" getDeclReferenced sl v
route (DBGetHierarchy !sl !v)                    = query "hierarchy" getHierarchy sl v
route (DBUpdateAndFindDirtyInclusions !s !is !v) = updateAndFindDirtyInclusions s is v
route (DBShutdown)                               = error "Should not route DBShutdown"

update :: Show a => String -> (DBHandle -> a -> IO ()) -> a -> DB ()
update item f x = do
  h <- dbHandle <$> ask
  logDebug $ "Updating index with " ++ item ++ ": " ++ show x
  lift $ f h x

update2 :: (Show a, Show b) => String -> (DBHandle -> a -> b -> IO ()) -> a -> b -> DB ()
update2 item f x y = do
  h <- dbHandle <$> ask
  logDebug $ "Updating index with " ++ item ++ ": " ++ show x ++ " " ++ show y
  lift $ f h x y

query :: Show a => String -> (DBHandle -> a -> IO b) -> a -> Response b -> DB ()
query item f x r = do
  h <- dbHandle <$> ask
  logDebug $ "Getting " ++ item ++ " for " ++ show x
  sendResponse r =<< lift (f h x)

getCommandInfoQuery :: SourceFile -> Response (Maybe CommandInfo, Maybe Time) -> DB ()
getCommandInfoQuery f r = do
  h <- dbHandle <$> ask
  let sfHash = hash f
  logDebug $ "Getting CommandInfo for " ++ show f
  info <- lift $ getCommandInfo h sfHash
  mtime <- lift $ getLastMTime h sfHash
  sendResponse r (info, mtime)

getSimilarCommandInfoQuery :: SourceFile -> Response (Maybe CommandInfo) -> DB ()
getSimilarCommandInfoQuery f v = do
  h <- dbHandle <$> ask
  let sfHash = hash f
  logDebug $ "Getting similar CommandInfo for " ++ show f
  info <- lift $ getCommandInfo h sfHash
  case info of
    Just ci -> sendResponse v $ Just ci
    Nothing -> sendResponse v =<< lift (getSimilarCommandInfo h f)

data InclusionDirtiness = NewInclusion Time
                        | InclusionChanged Time
                        | InclusionDepChanged
                        | InclusionUnchanged
                        | InclusionUnreadable
                          deriving (Eq, Show)
                                   
inclusionDirtiness :: SourceFile -> DB InclusionDirtiness
inclusionDirtiness sf = do
    ctx <- ask
    mayDirtiness <- lift $ atomically $ dirtinessCacheLookup (dbIndexStream ctx) sf
    case mayDirtiness of
      Just HasChangedDep -> return InclusionDepChanged
      Just HasNotChanged -> return InclusionUnchanged
      Nothing            -> inclusionDirtiness'
  where
    inclusionDirtiness' :: DB InclusionDirtiness
    inclusionDirtiness' = do
      ctx <- ask
      mayLastMTime <- lift $ getLastMTime (dbHandle ctx) (hash sf)
      mayMTime <- lift $ getMTime sf
      case (mayLastMTime, mayMTime) of
        (_, Nothing)                 -> return InclusionUnreadable
        (Nothing, Just mtime)        -> return $ NewInclusion mtime
        (Just lastMTime, Just mtime)
          | lastMTime /= mtime       -> return $ InclusionChanged mtime
          | otherwise                -> return InclusionUnchanged
      
updateAndFindDirtyInclusions :: SourceFileHash -> [Inclusion] -> Response [SourceFileHash]
                             -> DB ()
updateAndFindDirtyInclusions sfHash is v = do
    ctx <- ask
    let iStream = dbIndexStream ctx

    -- Grab the lock on as many files as possible. If any of the
    -- inclusions are already locked, another thread will take care of
    -- them, so we'll only worry about the remaining ones.
    currentIncs <- lift $ atomically $ acquireInclusions iStream sfHash is

    -- Process the inclusions and determine which inclusions are actually dirty.
    -- For inclusions which *are* dirty, we update the dirtiness cache to mark
    -- them as not having changed, since we're about to index them.
    (dirtyIncs, cleanIncs, reqs) <- foldM checkDirtiness ([], [], []) currentIncs

    -- We update the dirtiness cache to mark each inclusion as not having changed,
    -- since either that's true now or we're about to index them.
    forM_ currentIncs $ \(!ic, _) ->
      lift $ atomically $ updateDirtinessCache iStream (icInclusion ic) HasNotChanged

    -- Add requests to insert all of the inclusions.
    let reqs' = foldl' (\rs ic -> DBUpdateInclusion ic : rs) reqs is

    -- Send the requests.
    lift $ writeLenChan (dbUpdateChan ctx) $ reverse reqs'
    
    -- Release the lock on the clean inclusions.
    lift $ atomically $ releaseInclusions (dbIndexStream ctx) sfHash cleanIncs

    -- Return a list of dirty inclusions to index.
    sendResponse v dirtyIncs

  where

    checkDirtiness (!cleanIncs, !dirtyIncs, !reqs) (!ic, !icHash) = do
      let icSF = icInclusion ic
          returnDirty ty mayT = do logInfo $ "Will index " ++ ty ++ " inclusion " ++ show icSF
                                   return (cleanIncs, icHash : dirtyIncs,
                                           preIndexingUpdates icSF icHash mayT ++ reqs)
          returnClean = return (icHash : cleanIncs, dirtyIncs, reqs)

      dirtiness <- inclusionDirtiness icSF
      case dirtiness of
          NewInclusion t      -> returnDirty "new" $ Just t
          InclusionChanged t  -> returnDirty "modified" $ Just t
          InclusionDepChanged -> returnDirty "for dependency change" Nothing
          InclusionUnreadable -> returnDirty "unreadable" $ Just 0
          InclusionUnchanged  -> returnClean

preIndexingUpdates :: SourceFile -> SourceFileHash -> Maybe Time -> [DBUpdate]
preIndexingUpdates sf sfHash mayT = reverse $
  -- Reset the metadata for this inclusion.
  DBResetMetadata sf :

  -- Add a special definition for the beginning of each inclusion.
  -- A corresponding ref is added for inclusion directives.
  DBUpdateDef (DefUpdate sf sfHash sfHash 1 1 SourceFile 0) :

  -- If we have a new mtime, also update that.
  case mayT of
    Just t  -> [DBUpdateFile sf t]
    Nothing -> []
