module Pygmalion.Analysis.Manager
( runAnalysisManager
, AnalysisRequest (..)
, AnalysisChan
) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import qualified Data.Text as T
import System.Directory

import Pygmalion.Analysis.Source
import Pygmalion.Core
import Pygmalion.Database.Manager

data AnalysisRequest = Analyze CommandInfo
                     | ShutdownAnalysis
type AnalysisChan = Chan AnalysisRequest

runAnalysisManager :: AnalysisChan -> DBChan -> IO ()
runAnalysisManager chan dbChan = do
    wd <- T.pack <$> getCurrentDirectory
    sas <- mkSourceAnalysisState wd
    go sas
  where go :: SourceAnalysisState -> IO ()
        go sas = {-# SCC "analysisThread" #-}
               do req <- readChan chan
                  case req of
                      Analyze cmd -> scanCommandAndSendToDB sas cmd dbChan >> go sas
                      ShutdownAnalysis -> return ()

scanCommandAndSendToDB :: SourceAnalysisState -> CommandInfo -> DBChan -> IO ()
scanCommandAndSendToDB sas cmdInfo dbChan = do
  result <- analyzeCode sas cmdInfo
  when (isJust result) $ writeChan dbChan (DBUpdate . fromJust $ result)

analyzeCode :: SourceAnalysisState -> CommandInfo -> IO (Maybe SourceAnalysisResult)
analyzeCode sas ci = do
  liftIO $ putStrLn $ "Analyzing " ++ (show . ciSourceFile $ ci)
  result <- liftIO $ runSourceAnalyses sas ci
  case result of
    Just (is, ds) -> return . Just $! ci `seq` is `seq` ds `seq` (ci, is, ds)
    Nothing       -> return $! Nothing
