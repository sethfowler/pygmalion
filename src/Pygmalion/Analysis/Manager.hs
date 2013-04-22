module Pygmalion.Analysis.Manager
( runAnalysisManager
, AnalysisRequest (..)
, AnalysisChan
) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Char8 (ByteString)
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Process
import Data.DateTime
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Serialize
import System.Directory

import Control.Concurrent.Chan.Counting
import qualified Pygmalion.Analysis.ClangRequest as CR
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Log

data AnalysisRequest = Analyze CommandInfo
                     | AnalyzeSource SourceFile
                     | ShutdownAnalysis
type AnalysisChan = CountingChan AnalysisRequest

runAnalysisManager :: AnalysisChan -> DBChan -> IO ()
runAnalysisManager aChan dbChan = do
    let indexer = conduitProcess (proc "pygclangindex" []) :: Indexer
    go indexer
  where
    go indexer = {-# SCC "analysisThread" #-} do
                  req <- readCountingChan aChan
                  newCount <- getChanCount aChan
                  logDebug $ "Analysis channel now has " ++ (show newCount) ++ " items waiting"
                  case req of
                      Analyze cmd      -> doAnalyze aChan dbChan indexer cmd >> go indexer
                      AnalyzeSource sf -> doAnalyzeSource aChan dbChan indexer sf >> go indexer
                      ShutdownAnalysis -> logInfo "Shutting down analysis thread"

type Indexer = Conduit ByteString (ResourceT IO) ByteString

getMTime :: SourceFile -> IO Time
getMTime sf = do
  result <- try $ getModificationTime (unSourceFile sf)
  case result of
    Right clockTime -> return $ floor (utcTimeToPOSIXSeconds . fromClockTime $ clockTime)
    Left e          -> do logInfo $ "Couldn't read mtime for file " ++ (unSourceFile sf) ++ ": " ++ (show (e :: IOException))
                          return 0  -- Most likely the file has been deleted.

doAnalyze :: AnalysisChan -> DBChan -> Indexer -> CommandInfo -> IO ()
doAnalyze aChan dbChan indexer ci = do
    -- FIXME: Add an abstraction over this pattern.
    mOldCI <- newEmptyMVar
    writeCountingChan dbChan (DBGetCommandInfo (ciSourceFile ci) mOldCI)
    oldCI <- takeMVar mOldCI 
    mtime <- getMTime (ciSourceFile ci)
    when (isJust oldCI) $ logInfo ("Deciding whether to analyze file with index time: " ++ (show . ciLastIndexed . fromJust $ oldCI))
    case oldCI of
      Just oldCI' | (ciLastIndexed oldCI') < mtime -> doAnalyze'
                  | otherwise                      -> do updateCommand dbChan ci
                                                         logInfo $ "Index is up-to-date for built file " ++ (show . ciSourceFile $ ci) ++ " (file mtime: " ++ (show mtime) ++ ")"
      _                                            -> doAnalyze'
  where
    doAnalyze' = do toReindex <- filesToReindex dbChan ci
                    forM_ toReindex $ \f -> analyzeCode aChan dbChan indexer f
                    updateCommand dbChan ci

doAnalyzeSource :: AnalysisChan -> DBChan -> Indexer -> SourceFile -> IO ()
doAnalyzeSource aChan dbChan indexer sf = do
    -- FIXME: Add an abstraction over this pattern.
    mOldCI <- newEmptyMVar
    writeCountingChan dbChan (DBGetSimilarCommandInfo sf mOldCI)
    oldCI <- takeMVar mOldCI 
    case oldCI of
      Just oldCI' -> do mtime <- getMTime (ciSourceFile oldCI')
                        doAnalyzeSource' oldCI' mtime
      _           -> logInfo $ "Not indexing unknown file " ++ (show sf)
  where
    doAnalyzeSource' ci mt | (ciLastIndexed ci) < mt = do toReindex <- filesToReindex dbChan ci
                                                          forM_ toReindex $ \f -> analyzeCode aChan dbChan indexer f
                           | otherwise               = logInfo $ "Index is up-to-date for file " ++ (show sf)

-- If the source file associated with this CommandInfo has changed, what must
-- we reindex?
filesToReindex :: DBChan -> CommandInfo -> IO [CommandInfo]
filesToReindex dbChan ci = do
  mIncluders <- newEmptyMVar
  writeCountingChan dbChan (DBGetIncluders (ciSourceFile ci) mIncluders)
  includers <- takeMVar mIncluders 
  return (ci : includers)

updateCommand :: DBChan -> CommandInfo -> IO ()
updateCommand dbChan ci = writeCountingChan dbChan (DBUpdateCommandInfo ci)

analyzeHeader :: AnalysisChan -> DBChan -> CommandInfo -> Inclusion -> IO ()
analyzeHeader aChan dbChan ci ic = do
    mOldCI <- newEmptyMVar
    writeCountingChan dbChan (DBGetCommandInfo (icSourceFile ic) mOldCI)
    oldCI <- takeMVar mOldCI 
    mtime <- getMTime (icSourceFile ic)
    case oldCI of
      Just oldCI' | (ciLastIndexed oldCI') < mtime -> doAnalyze'
                  | otherwise                      -> do logInfo $ "Index is up-to-date for header file " ++ (show . icSourceFile $ ic) ++ " (file mtime: " ++ (show mtime) ++ ")"
      _ -> doAnalyze'
  where
    doAnalyze' = writeCountingChan aChan $ Analyze ci { ciLastIndexed = 0, ciSourceFile = icHeaderFile ic}

analyzeCode :: AnalysisChan -> DBChan -> Indexer -> CommandInfo -> IO ()
analyzeCode aChan dbChan indexer ci = do
    liftIO $ logInfo $ "Indexing " ++ (show . ciSourceFile $ ci)
    runResourceT (source $= conduitPut putReq =$= indexer =$= conduitGet getResp $$ process)
  where
    source = yield (CR.Analyze ci) >> yield (CR.Shutdown)
    putReq :: Putter CR.ClangRequest
    putReq = put
    getResp = get :: Get CR.ClangResponse
    process = do
      liftIO $ logDebug "WAITING"
      resp <- await
      case resp of
        Just (CR.FoundDef di)       -> liftIO (writeCountingChan dbChan (DBUpdateDefInfo di)) >> process
        Just (CR.FoundOverride ov)  -> liftIO (writeCountingChan dbChan (DBUpdateOverride ov)) >> process
        Just (CR.FoundCaller cr)    -> liftIO (writeCountingChan dbChan (DBUpdateCaller cr)) >> process
        Just (CR.FoundRef rf)       -> liftIO (writeCountingChan dbChan (DBUpdateRef rf)) >> process
        Just (CR.FoundInclusion ic) -> do liftIO (writeCountingChan dbChan (DBUpdateInclusion ci ic))
                                          when (icDirect ic) (liftIO $ analyzeHeader aChan dbChan ci ic)
                                          process
        Just (CR.EndOfAnalysis)     -> liftIO (logDebug "Done reading from clang process") >> return ()
        Nothing                     -> liftIO (logDebug "Clang process read failed") >> return ()
