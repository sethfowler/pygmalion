module Pygmalion.Analyze
( runAnalysisThread
, runDatabaseThread
, AnalysisRequest (..)
, AnalysisChan
, DBRequest (..)
, DBChan
) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import qualified Data.Text as T
import System.Directory

import Pygmalion.Analyze.Source
import Pygmalion.Core
import Pygmalion.Database

type SourceAnalysisResult = (CommandInfo, [SourceFile], [DefInfo])

data AnalysisRequest = Analyze CommandInfo
                     | ShutdownAnalysis
type AnalysisChan = Chan AnalysisRequest

data DBRequest = DBUpdate SourceAnalysisResult
               | DBGetCommandInfo SourceFile (MVar (Maybe CommandInfo))
               | DBGetDefinition USR (MVar (Maybe DefInfo))
               | DBShutdown
type DBChan = Chan DBRequest
    

runAnalysisThread :: AnalysisChan -> DBChan -> IO ()
runAnalysisThread chan dbChan = do
    wd <- T.pack <$> getCurrentDirectory
    sas <- mkSourceAnalysisState wd
    go sas
  where go :: SourceAnalysisState -> IO ()
        go sas = {-# SCC "analysisThread" #-}
               do req <- readChan chan
                  case req of
                      Analyze cmd -> scanCommandAndSendToDBThread sas cmd dbChan >> go sas
                      ShutdownAnalysis -> return ()

-- FIXME: It'd be nice to have separate chans for queries and update requests
-- or something similar, to allow queries to have higher priority.
runDatabaseThread :: DBChan -> IO ()
runDatabaseThread chan = withDB go
  where go :: DBHandle -> IO ()
        go h = {-# SCC "databaseThread" #-}
               do req <- readChan chan
                  case req of
                    DBUpdate sar         -> doUpdate h sar >> go h
                    DBGetCommandInfo f v -> doGetCommandInfo h f v >> go h
                    DBGetDefinition u v  -> doGetDefinition h u v >> go h
                    DBShutdown           -> return ()

scanCommandAndSendToDBThread :: SourceAnalysisState -> CommandInfo -> DBChan -> IO ()
scanCommandAndSendToDBThread sas cmdInfo dbChan = do
  result <- analyzeCode sas cmdInfo
  when (isJust result) $ writeChan dbChan (DBUpdate . fromJust $ result)

analyzeCode :: SourceAnalysisState -> CommandInfo -> IO (Maybe SourceAnalysisResult)
analyzeCode sas ci = do
  --liftIO $ putStrLn $ "Analyzing " ++ (show ci)
  result <- liftIO $ runSourceAnalyses sas ci
  case result of
    Just (is, ds) -> return . Just $ (ci, is, ds)
    Nothing       -> return Nothing

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
