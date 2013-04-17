module Pygmalion.Analysis.Manager
( runAnalysisManager
, AnalysisRequest (..)
, AnalysisChan
) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Char8 (ByteString)
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Process
import Data.Maybe
import Data.Serialize
import qualified Data.Text as T
import System.Directory

import Control.Concurrent.Chan.Counting
import qualified Pygmalion.Analysis.ClangRequest as CR
import Pygmalion.Analysis.Source
import Pygmalion.Core
import Pygmalion.Database.Manager

data AnalysisRequest = Analyze CommandInfo
                     | AnalyzeSource SourceFile Time
                     | ShutdownAnalysis
type AnalysisChan = CountingChan AnalysisRequest

runAnalysisManager :: AnalysisChan -> DBChan -> IO ()
runAnalysisManager chan dbChan = do
    let indexer = conduitProcess (proc "pygclangindex" []) :: Indexer
    go indexer
  where
    go indexer = {-# SCC "analysisThread" #-} do
                  req <- readCountingChan chan
                  newCount <- getChanCount chan
                  putStrLn $ "Analysis channel now has " ++ (show newCount) ++ " items waiting"
                  case req of
                      Analyze cmd        -> doAnalyze indexer cmd dbChan >> go indexer
                      AnalyzeSource sf t -> doAnalyzeSource indexer sf t dbChan >> go indexer
                      ShutdownAnalysis   -> putStrLn "Shutting down analysis thread"

type Indexer = Conduit ByteString (ResourceT IO) ByteString

doAnalyze :: Indexer -> CommandInfo -> DBChan -> IO ()
doAnalyze indexer ci dbChan = do
    -- FIXME: Add an abstraction over this pattern.
    mOldCI <- newEmptyMVar
    writeCountingChan dbChan (DBGetCommandInfo (ciSourceFile ci) mOldCI)
    oldCI <- takeMVar mOldCI 
    case oldCI of
      Just (CommandInfo _ _ _ oldT) | (ciBuildTime ci) <= oldT -> putStrLn $ "Skipping analysis for " ++ (show . ciSourceFile $ ci)
      _                                                        -> doAnalyze'
  where
    doAnalyze' = analyzeCode indexer dbChan ci

doAnalyzeSource :: Indexer -> SourceFile -> Time -> DBChan -> IO ()
doAnalyzeSource indexer sf t dbChan = do
    -- FIXME: Add an abstraction over this pattern.
    mOldCI <- newEmptyMVar
    writeCountingChan dbChan (DBGetSimilarCommandInfo sf mOldCI)
    oldCI <- takeMVar mOldCI 
    case oldCI of
      Just (CommandInfo _ _ _ oldT) | t <= oldT -> putStrLn $ "Skipping analysis for " ++ (show sf)
      Just ci                                   -> doAnalyzeSource' $ ci { ciSourceFile = sf }
      _                                         -> putStrLn $ "Skipping analysis for " ++ (show sf)
  where
    doAnalyzeSource' ci = analyzeCode indexer dbChan ci

analyzeCode ::  Indexer -> DBChan -> CommandInfo -> IO ()
analyzeCode indexer dbChan ci = do
    liftIO $ putStrLn $ "Analyzing " ++ (show . ciSourceFile $ ci) ++ " [" ++ (show . ciBuildTime $ ci) ++ "]"
    runResourceT (source $= conduitPut putReq =$= indexer =$= conduitGet getResp $$ process)
    writeCountingChan dbChan (DBUpdateCommandInfo ci)
  where
    source = yield (CR.Analyze ci) >> yield (CR.Shutdown)
    putReq :: Putter CR.ClangRequest
    putReq = put
    getResp = get :: Get CR.ClangResponse
    process = do
      liftIO $ putStrLn "WAITING"
      resp <- await
      case resp of
        Just (CR.FoundDef di) -> liftIO (writeCountingChan dbChan (DBUpdateDefInfo di)) >> process
        Just (CR.EndOfDefs)   -> liftIO (putStrLn "Done reading from clang process") >> return ()
        Nothing               -> liftIO (putStrLn "Clang process read failed") >> return ()
