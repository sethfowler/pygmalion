{-# LANGUAGE BangPatterns #-}

module Pygmalion.Database.Manager
( runDatabaseManager
, ensureDB
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Data.Time.Clock

import Control.Concurrent.Chan.Len
import Pygmalion.Core
import Pygmalion.Database.IO
import Pygmalion.Database.Request
import Pygmalion.Index.Extension
import Pygmalion.Index.Request
import Pygmalion.Index.Stream
import Pygmalion.Log

runDatabaseManager :: DBChan -> DBChan -> IndexStream -> IO ()
runDatabaseManager chan queryChan iStream = do
    start <- getCurrentTime
    withDB $ \h -> do
      let ctx = DBContext h iStream
      go ctx 0 start
  where
    go :: DBContext -> Int -> UTCTime -> IO ()
    go !ctx 1000 !start = do 
      stop <- getCurrentTime
      logInfo $ "Handled 1000 records in " ++ show (stop `diffUTCTime` start)
      newStart <- getCurrentTime
      go ctx 0 newStart
    go !ctx !n !s = {-# SCC "databaseThread" #-}
           do !req <- readLenChanPreferFirst queryChan chan
              case req of
                DBShutdown -> logInfo "Shutting down DB thread"
                _          -> runReaderT (route req) ctx >> go ctx (n+1) s
                
data DBContext = DBContext
  { dbHandle      :: !DBHandle
  , dbIndexStream :: !IndexStream
  }
type DB a = ReaderT DBContext IO a

route :: DBRequest -> DB ()
route (DBUpdateCommandInfo !ci)        = update "command info" updateSourceFile ci
route (DBUpdateDef !di)                = update "definition" updateDef di
route (DBUpdateOverride !ov)           = update "override" updateOverride ov
route (DBUpdateRef !rf)                = update "reference" updateReference rf
route (DBUpdateInclusion !ic)          = updateInclusionAndIndex ic
route (DBResetMetadata !sf)            = update "resetted metadata" resetMetadata sf
route (DBGetCommandInfo !f !v)         = query "command info" getCommandInfo f v
route (DBGetSimilarCommandInfo !f !v)  = getSimilarCommandInfoQuery f v
route (DBGetDefinition !sl !v)         = query "definition" getDef sl v
route (DBGetInclusions !sf !v)         = query "inclusions" getInclusions sf v
route (DBGetIncluders !sf !v)          = query "includers" getIncluders sf v
route (DBGetIncluderInfo !sf !v)       = query "includer info" getIncluderInfo sf v
route (DBGetInclusionHierarchy !sf !v) = query "inclusion hierarchy"
                                               getInclusionHierarchy sf v
route (DBGetCallers !sl !v)            = query "callers" getCallers sl v
route (DBGetCallees !usr !v)           = query "callees" getCallees usr v
route (DBGetBases !usr !v)             = query "bases" getOverrided usr v
route (DBGetOverrides !usr !v)         = query "overrides" getOverriders usr v
route (DBGetMembers !usr !v)           = query "members" getMembers usr v
route (DBGetRefs !usr !v)              = query "references" getReferences usr v
route (DBGetReferenced !sl !v)         = query "referenced" getReferenced sl v
route (DBGetDeclReferenced !sl !v)     = query "decl referenced" getDeclReferenced sl v
route (DBGetHierarchy !sl !v)          = query "hierarchy" getHierarchy sl v
route (DBShutdown)                     = error "Should not route DBShutdown"

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

updateInclusionAndIndex :: Inclusion -> DB ()
updateInclusionAndIndex ic = do
  ctx <- ask
  lift $ updateInclusion (dbHandle ctx) ic
  -- Only request indexing for an inclusion if a source file included
  -- it. It doesn't make sense to do it otherwise since source files
  -- request indexing for all of their transitive inclusions at once.
  when (hasSourceExtensionBS $ icSourceFile ic) $ do
    lift $ atomically $ addPendingIndex (dbIndexStream ctx) $ FromBuild (icCommandInfo ic) False
