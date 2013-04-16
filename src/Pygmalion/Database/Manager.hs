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

import Pygmalion.Analysis.Source
import Pygmalion.Core
import Pygmalion.Database.IO

data DBRequest = DBUpdate SourceAnalysisResult
               | DBGetCommandInfo SourceFile (MVar (Maybe CommandInfo))
               | DBGetDefinition USR (MVar (Maybe DefInfo))
               | DBShutdown
type DBChan = Chan DBRequest
    
-- FIXME: It'd be nice to have separate chans for queries and update requests
-- or something similar, to allow queries to have higher priority.
runDatabaseManager :: DBChan -> IO ()
runDatabaseManager chan = withDB go
  where go :: DBHandle -> IO ()
        go h = {-# SCC "databaseThread" #-}
               do req <- readChan chan
                  case req of
                    DBUpdate sar         -> doUpdate h sar >> go h
                    DBGetCommandInfo f v -> doGetCommandInfo h f v >> go h
                    DBGetDefinition u v  -> doGetDefinition h u v >> go h
                    DBShutdown           -> putStrLn "Shutting down DB thread"

doUpdate :: DBHandle -> SourceAnalysisResult -> IO ()
doUpdate h (ci, includes, defs) = liftIO $ withTransaction h $ do
  updateSourceFile h ci
  -- Update entries for all non-system includes, using the same metadata.
  -- forM_ includes $ \i -> do
    -- updateSourceFile h $ withSourceFile ci i
  -- Update entries for all definitions.
  forM_ defs $ \d -> do
    updateDef h d
  --liftIO $ putStrLn $ "Updated DB entries related to " ++ (show ci)

doGetCommandInfo :: DBHandle -> SourceFile -> MVar (Maybe CommandInfo) -> IO ()
doGetCommandInfo h f v = do
  ci <- liftM2 (<|>) (getCommandInfo h f) (getSimilarCommandInfo h f)
  putMVar v $! ci

doGetDefinition :: DBHandle -> USR -> MVar (Maybe DefInfo) -> IO ()
doGetDefinition h usr v = do
  def <- getDef h usr
  putMVar v $! def