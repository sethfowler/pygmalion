{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Database.Manager
( runDatabaseManager
, ensureDB
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Vector as V
import qualified System.Remote.Monitoring as EKG
import qualified System.Remote.Gauge as Gauge

import Control.Concurrent.Chan.Len
import Pygmalion.Core
import Pygmalion.Database.IO
import Pygmalion.Database.Request
import Pygmalion.Log

runDatabaseManager :: DBUpdateChan -> DBQueryChan -> EKG.Server -> IO ()
runDatabaseManager updateChan queryChan ekg = do
    updateGauge <- EKG.getGauge "Database Updates Popped" ekg
    withDB $ \h -> do
      let ctx = DBContext h updateGauge
      go ctx
  where
    go :: DBContext -> IO ()
    go !ctx = 
           do !item <- atomically $ readFromChannels updateChan queryChan
              case item of
                Left ups         -> routeUpdates ctx (reverse ups) >> go ctx
                Right DBShutdown -> logInfo "Shutting down DB thread"
                Right req        -> runReaderT (route req) ctx >> go ctx
                
readFromChannels :: DBUpdateChan -> DBQueryChan -> STM (Either [V.Vector DBUpdate] DBRequest)
readFromChannels updateChan queryChan = readQueryChan `orElse` readUpdateChan
  where
    readQueryChan  = Right <$> readTBQueue queryChan
    readUpdateChan = Left <$> popUpdates updateChan

data DBContext = DBContext
  { dbHandle      :: !DBHandle
  , dbUpdateGauge :: !Gauge.Gauge
  }
type DB a = ReaderT DBContext IO a

routeUpdates :: DBContext -> [V.Vector DBUpdate] -> IO ()
routeUpdates ctx upList = do
    Gauge.set (dbUpdateGauge ctx) $ sum (map V.length upList)
    withTransaction (dbHandle ctx) $
      forM_ upList $ \ups ->
        V.forM_ ups $ \up ->
          runReaderT (routeUpdate up) ctx
  where
    routeUpdate (DBUpdateDef !di)         = update "definition" stageDefUpdate di
    routeUpdate (DBUpdateRef !rf)         = update "reference" stageReferenceUpdate rf
    routeUpdate (DBUpdateOverride !ov)    = update "override" stageOverrideUpdate ov
    routeUpdate (DBUpdateCommandInfo !ci) = update "command info" updateSourceFile ci
    routeUpdate (DBUpdateFile !sf !t !vh) = update3 "file" updateFile sf t vh
    routeUpdate (DBUpdateInclusion !ic)   = update "inclusion" updateInclusion ic
    routeUpdate (DBResetMetadata !sf)     = update "resetted metadata" resetMetadata sf
    
route :: DBRequest -> DB ()
route (DBGetCommandInfo !f !v)                   = getCommandInfoQuery f v
route (DBGetSimilarCommandInfo !f !v)            = getSimilarCommandInfoQuery f v
route (DBGetDefinition !sl !v)                   = query "definition" getDef sl v
route (DBGetInclusions !sf !v)                   = query "inclusions" getInclusions sf v
route (DBGetIncluders !sf !v)                    = query "includers" getIncluders sf v
route (DBGetDirectIncluders !sf !v)              = query "direct includers" getDirectIncluders sf v
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
route (DBCommitStagedUpdates !v)                 = commit v
route (DBShutdown)                               = error "Should not route DBShutdown"

commit :: Response () -> DB ()
commit r = do
  h <- dbHandle <$> ask
  lift $ commitStagedUpdates h
  sendResponse r ()
  
update :: Show a => String -> (DBHandle -> a -> IO ()) -> a -> DB ()
update item f x = do
  h <- dbHandle <$> ask
  logDebug $ "Updating index with " ++ item ++ ": " ++ show x
  lift $ f h x

update3 :: (Show a, Show b, Show c) => String -> (DBHandle -> a -> b -> c -> IO ())
        -> a -> b -> c -> DB ()
update3 item f x y z = do
  h <- dbHandle <$> ask
  logDebug $ "Updating index with " ++ item ++ ": " ++ show x ++ " " ++ show y ++ " " ++ show z
  lift $ f h x y z

query :: Show a => String -> (DBHandle -> a -> IO b) -> a -> Response b -> DB ()
query item f x r = do
  h <- dbHandle <$> ask
  logDebug $ "Getting " ++ item ++ " for " ++ show x
  sendResponse r =<< lift (f h x)

getCommandInfoQuery :: SourceFile -> Response (Maybe CommandInfo, Maybe Time) -> DB ()
getCommandInfoQuery f r = do
  h <- dbHandle <$> ask
  let sfHash = stableHash f
  logDebug $ "Getting CommandInfo for " ++ show f
  info <- lift $ getCommandInfo h sfHash
  mtime <- lift $ getLastMTime h sfHash
  sendResponse r (info, mtime)

getSimilarCommandInfoQuery :: SourceFile -> Response (Maybe CommandInfo) -> DB ()
getSimilarCommandInfoQuery f v = do
  h <- dbHandle <$> ask
  let sfHash = stableHash f
  logDebug $ "Getting similar CommandInfo for " ++ show f
  info <- lift $ getCommandInfo h sfHash
  case info of
    Just ci -> sendResponse v $ Just ci
    Nothing -> sendResponse v =<< lift (getSimilarCommandInfo h f)
