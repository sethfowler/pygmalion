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

import Control.Concurrent.Chan.Counting
import Pygmalion.Analysis.Source
import Pygmalion.Core
import Pygmalion.Database.Manager

data AnalysisRequest = Analyze CommandInfo
                     | ShutdownAnalysis
type AnalysisChan = CountingChan AnalysisRequest

runAnalysisManager :: AnalysisChan -> DBChan -> IO ()
runAnalysisManager chan dbChan = do
    wd <- T.pack <$> getCurrentDirectory
    sas <- mkSourceAnalysisState wd
    go sas
  where go :: SourceAnalysisState -> IO ()
        go sas = {-# SCC "analysisThread" #-}
               do req <- readCountingChan chan
                  newCount <- getChanCount chan
                  putStrLn $ "Analysis channel now has " ++ (show newCount) ++ " items waiting"
                  case req of
                      Analyze cmd -> scanCommandAndSendToDB sas cmd dbChan >> go sas
                      ShutdownAnalysis -> putStrLn "Shutting down analysis thread"

scanCommandAndSendToDB :: SourceAnalysisState -> CommandInfo -> DBChan -> IO ()
scanCommandAndSendToDB sas ci dbChan = do
  result <- analyzeCode sas ci
  when (isJust result) $ writeCountingChan dbChan (DBUpdate . fromJust $ result)

analyzeCode :: SourceAnalysisState -> CommandInfo -> IO (Maybe SourceAnalysisResult)
analyzeCode sas ci = do
  liftIO $ putStrLn $ "Analyzing " ++ (show . ciSourceFile $ ci) ++ " [" ++ (show . ciBuildTime $ ci) ++ "]"
  result <- liftIO $ runSourceAnalyses sas ci
  case result of
    Just (is, ds) -> return . Just $! ci `seq` is `seq` ds `seq` (ci, is, ds)
    Nothing       -> return $! Nothing
