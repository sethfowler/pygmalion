module Pygmalion.Database.Manager
( runDatabaseManager
, ensureDB
, DBRequest (..)
, DBChan
) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Time.Clock.POSIX

import Control.Concurrent.Chan.Counting
import Pygmalion.Core
import Pygmalion.Database.IO
import Pygmalion.Log

data DBRequest = DBUpdateCommandInfo CommandInfo
               | DBUpdateDefInfo DefInfo
               | DBUpdateOverride Override
               | DBUpdateCaller Caller
               | DBUpdateRef Reference
               | DBUpdateInclusion Inclusion
               | DBGetCommandInfo SourceFile (MVar (Maybe CommandInfo))
               | DBGetSimilarCommandInfo SourceFile (MVar (Maybe CommandInfo))
               | DBGetDefinition USR (MVar (Maybe DefInfo))
               | DBShutdown
type DBChan = CountingChan DBRequest
    
-- FIXME: It'd be nice to have separate chans for queries and update requests
-- or something similar, to allow queries to have higher priority.
runDatabaseManager :: DBChan -> IO ()
runDatabaseManager chan = withDB go
  where go :: DBHandle -> IO ()
        go h = {-# SCC "databaseThread" #-}
               do req <- readCountingChan chan
                  newCount <- getChanCount chan
                  logDebug $ "Database channel now has " ++ (show newCount) ++ " items waiting"
                  case req of
                    DBUpdateCommandInfo ci      -> doUpdateCommandInfo h ci >> go h
                    DBUpdateDefInfo di          -> doUpdateDefInfo h di >> go h
                    DBUpdateOverride ov         -> doUpdateOverride h ov >> go h
                    DBUpdateCaller cr           -> doUpdateCaller h cr >> go h
                    DBUpdateRef rf              -> doUpdateRef h rf >> go h
                    DBUpdateInclusion ic        -> doUpdateInclusion h ic >> go h
                    DBGetCommandInfo f v        -> doGetCommandInfo h f v >> go h
                    DBGetSimilarCommandInfo f v -> doGetSimilarCommandInfo h f v >> go h
                    DBGetDefinition u v         -> doGetDefinition h u v >> go h
                    DBShutdown                  -> logInfo "Shutting down DB thread"

doUpdateCommandInfo :: DBHandle -> CommandInfo -> IO ()
doUpdateCommandInfo h cmd = liftIO $ withTransaction h $ do
  time <- getPOSIXTime
  let ci = cmd { ciLastIndexed = floor time }
  liftIO $ logDebug $ "Updating database with command: " ++ (show . ciSourceFile $ ci)
  updateSourceFile h ci

doUpdateDefInfo :: DBHandle -> DefInfo -> IO ()
doUpdateDefInfo h di = liftIO $ withTransaction h $ do
  liftIO $ logDebug $ "Updating database with def: " ++ (show . diUSR $ di)
  updateDef h di

doUpdateOverride :: DBHandle -> Override -> IO ()
doUpdateOverride h ov = liftIO $ withTransaction h $ do
  liftIO $ logDebug $ "Updating database with override: " ++ (show ov)
  updateOverride h ov

doUpdateCaller :: DBHandle -> Caller -> IO ()
doUpdateCaller h cr = liftIO $ withTransaction h $ do
  liftIO $ logDebug $ "Updating database with caller: " ++ (show cr)
  updateCaller h cr

doUpdateRef :: DBHandle -> Reference -> IO ()
doUpdateRef h rf = liftIO $ withTransaction h $ do
  liftIO $ logDebug $ "Updating database with reference: " ++ (show rf)
  updateReference h rf

doUpdateInclusion :: DBHandle -> Inclusion -> IO ()
doUpdateInclusion h ic = liftIO $ withTransaction h $ do
  liftIO $ logDebug $ "Updating database with inclusion: " ++ (show ic)
  updateInclusion h ic

doGetCommandInfo :: DBHandle -> SourceFile -> MVar (Maybe CommandInfo) -> IO ()
doGetCommandInfo h f v = do
  liftIO $ logDebug $ "Getting CommandInfo for " ++ (show f)
  ci <- getCommandInfo h f
  putMVar v $! ci

doGetSimilarCommandInfo :: DBHandle -> SourceFile -> MVar (Maybe CommandInfo) -> IO ()
doGetSimilarCommandInfo h f v = do
  liftIO $ logDebug $ "Getting similar CommandInfo for " ++ (show f)
  ci <- liftM2 (<|>) (getCommandInfo h f) (getSimilarCommandInfo h f)
  putMVar v $! ci

doGetDefinition :: DBHandle -> USR -> MVar (Maybe DefInfo) -> IO ()
doGetDefinition h usr v = do
  liftIO $ logDebug $ "Getting DefInfo for " ++ (show usr)
  def <- getDef h usr
  putMVar v $! def
