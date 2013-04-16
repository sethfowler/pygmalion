module Pygmalion.Analyze
( runAnalysisThread
, runDatabaseThread
) where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
import System.Directory

import Pygmalion.Analyze.Source
import Pygmalion.Core
import Pygmalion.Database

runAnalysisThread :: Chan (Maybe CommandInfo) -> Chan (Maybe (CommandInfo, [SourceFile], [DefInfo])) -> IO ()
runAnalysisThread chan dbChan = do
    wd <- T.pack <$> getCurrentDirectory
    sas <- mkSourceAnalysisState wd
    go sas
  where go :: SourceAnalysisState -> IO ()
        go sas = {-# SCC "analysisThread" #-}
               do mayCmd <- readChan chan
                  case mayCmd of
                      Just cmd -> scanCommandAndSendToDBThread sas cmd dbChan >> go sas
                      Nothing  -> return ()

runDatabaseThread :: Chan (Maybe (CommandInfo, [SourceFile], [DefInfo])) -> IO ()
runDatabaseThread chan = withDB go
  where go :: DBHandle -> IO ()
        go h = {-# SCC "databaseThread" #-}
               do mayInfo <- readChan chan
                  case mayInfo of
                    Just info -> updateDB h info >> go h
                    Nothing  -> return ()

scanCommandAndSendToDBThread :: SourceAnalysisState -> CommandInfo -> Chan (Maybe (CommandInfo, [SourceFile], [DefInfo])) -> IO ()
scanCommandAndSendToDBThread sas cmdInfo dbChan = do
  mayResult <- analyzeCode sas cmdInfo
  case mayResult of
    result@(Just _) -> writeChan dbChan result
    Nothing         -> return ()

analyzeCode :: SourceAnalysisState -> CommandInfo -> IO (Maybe (CommandInfo, [SourceFile], [DefInfo]))
analyzeCode sas ci = do
  --liftIO $ putStrLn $ "Analyzing " ++ (show ci)
  result <- liftIO $ runSourceAnalyses sas ci
  case result of
    Just (is, ds) -> return . Just $ (ci, is, ds)
    Nothing       -> return Nothing

updateDB :: DBHandle -> (CommandInfo, [SourceFile], [DefInfo]) -> IO ()
updateDB h (ci, includes, defs) = liftIO $ withTransaction h $ do
  updateSourceFile h ci
  -- Update entries for all non-system includes, using the same metadata.
  -- forM_ includes $ \i -> do
    -- updateSourceFile h $ withSourceFile ci i
  -- Update entries for all definitions.
  forM_ defs $ \d -> do
    updateDef h d
  --liftIO $ putStrLn $ "Updated DB entries related to " ++ (show ci)
