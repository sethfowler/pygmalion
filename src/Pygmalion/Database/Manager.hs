{-# LANGUAGE BangPatterns #-}

module Pygmalion.Database.Manager
( runDatabaseManager
, ensureDB
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Data.List (partition)
import Data.Time.Clock

import Control.Concurrent.Chan.Len
import Pygmalion.Core
import Pygmalion.Database.IO
import Pygmalion.Database.Request
import Pygmalion.Index.Request
import Pygmalion.Index.Stream
import Pygmalion.Log

runDatabaseManager :: DBUpdateChan -> DBQueryChan -> IndexStream -> IO ()
runDatabaseManager updateChan queryChan iStream = do
    start <- getCurrentTime
    withDB $ \h -> do
      let ctx = DBContext h iStream
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
  }
type DB a = ReaderT DBContext IO a

routeUpdates :: [DBUpdate] -> DB ()
routeUpdates ups = mapM_ routeUpdate ups
  where
    routeUpdate (DBUpdateDef !di)         = update "definition" updateDef di
    routeUpdate (DBUpdateRef !rf)         = update "reference" updateReference rf
    routeUpdate (DBUpdateOverride !ov)    = update "override" updateOverride ov
    routeUpdate (DBUpdateCommandInfo !ci) = update "command info" updateSourceFile ci
    routeUpdate (DBResetMetadata !sf)     = update "resetted metadata" resetMetadata sf
    
route :: DBRequest -> DB ()
route (DBGetCommandInfo !f !v)                   = query "command info" getCommandInfo f v
route (DBGetSimilarCommandInfo !f !v)            = getSimilarCommandInfoQuery f v
route (DBGetDefinition !sl !v)                   = query "definition" getDef sl v
route (DBGetInclusions !sf !v)                   = query "inclusions" getInclusions sf v
route (DBGetIncluders !sf !v)                    = query "includers" getIncluders sf v
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

query :: Show a => String -> (DBHandle -> a -> IO b) -> a -> Response b -> DB ()
query item f x r = do
  h <- dbHandle <$> ask
  logDebug $ "Getting " ++ item ++ " for " ++ show x
  sendResponse r =<< (lift $ f h x)

getSimilarCommandInfoQuery :: SourceFile -> Response (Maybe CommandInfo) -> DB ()
getSimilarCommandInfoQuery f v = do
  h <- dbHandle <$> ask
  logDebug $ "Getting similar CommandInfo for " ++ show f
  ci <- liftM2 (<|>) (lift $ getCommandInfo h f) (lift $ getSimilarCommandInfo h f)
  sendResponse v ci

updateAndFindDirtyInclusions :: SourceFileHash -> [Inclusion] -> Response [SourceFileHash]
                             -> DB ()
updateAndFindDirtyInclusions sfHash is v = do
  ctx <- ask
  let h = dbHandle ctx

  -- Insert all of the inclusions.
  forM_ is $ \ic ->
    lift $ updateInclusion h ic

  -- Grab the lock on as many files as possible. If any of the
  -- inclusions are already locked, another thread will take care of
  -- them, so we'll only worry about the remaining ones.
  currentIncs <- lift $ atomically $ acquireInclusions (dbIndexStream ctx) sfHash is

  -- Process the inclusions and determine which inclusions are actually dirty.
  taggedIncs <- forM currentIncs $ \(ic, icHash) -> do
    let icSF = icInclusion ic

    -- Add a special definition for the beginning of each file we're
    -- indexing. This is referenced by inclusion directives.
    lift (updateDef h $ DefUpdate icSF icHash icHash 1 1 SourceFile 0)

    mayCI <- lift $ atomically $ getLastIndexedCache (dbIndexStream ctx) icSF
    dirtiness <- lift $ fileDirtiness (FromInclusion icSF)
                                      (getCommandInfo (dbHandle ctx) icSF)
                                      mayCI
    case dirtiness of
      Unreadable         -> do logInfo (show ic ++ " is unreadable")
                               return (False, icHash)
      Unchanged t        -> do logInfo (show ic ++ " is unchanged")
                               lift (updateFile h icSF t 0)
                               return (False, icHash)
      NewInclusion t     -> do logInfo (show ic ++ " is new")
                               lift (updateFile h icSF t 0)
                               return (True, icHash)
      InclusionChanged t -> do logInfo (show ic ++ " has changed")
                               lift (updateFile h icSF t 0)
                               return (True, icHash)
      _                  -> error "Unexpected dirtiness when processing inclusion"

  let (dirtyIncs, cleanIncs) = partition fst taggedIncs

  -- Release the lock on the clean inclusions.
  lift $ atomically $ releaseInclusions (dbIndexStream ctx) sfHash (map snd cleanIncs)

  -- Return a list.
  sendResponse v (map snd dirtyIncs)
