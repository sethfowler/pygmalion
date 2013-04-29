{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Pygmalion.Analysis.Manager
( runAnalysisManager
, AnalysisRequest (..)
, AnalysisChan
) where

import Control.Concurrent hiding (yield)
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
import qualified Data.Set as Set
import System.Directory

import Control.Concurrent.Chan.Len
import qualified Pygmalion.Analysis.ClangRequest as CR
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Log

data AnalysisRequest = AnalyzeBuiltFile CommandInfo
                     | AnalyzeNotifiedFile SourceFile
                     | ShutdownAnalysis
type AnalysisChan = LenChan AnalysisRequest

runAnalysisManager :: AnalysisChan -> DBChan -> DBChan -> MVar (Set.Set SourceFile) -> IO ()
runAnalysisManager aChan dbChan dbQueryChan lox = do
    let indexer = conduitProcess (proc "pygclangindex" []) :: Indexer
    go indexer
  where
    go !indexer = {-# SCC "analysisThread" #-} do
                  (newCount, req) <- readLenChan aChan
                  logDebug $ "Analysis channel now has " ++ (show newCount) ++ " items waiting"
                  case req of
                      AnalyzeBuiltFile ci    -> checkLock lox (ciSourceFile ci) (doAnalyzeBuiltFile aChan dbChan dbQueryChan indexer ci) >> go indexer
                      AnalyzeNotifiedFile sf -> checkLock lox sf (doAnalyzeNotifiedFile aChan dbChan dbQueryChan indexer sf) >> go indexer
                      ShutdownAnalysis       -> logInfo "Shutting down analysis thread"

type Indexer = Conduit ByteString (ResourceT IO) ByteString

checkLock :: MVar (Set.Set SourceFile) -> SourceFile -> IO () -> IO ()
checkLock !lox !sf !action = do
  lockedFiles <- takeMVar lox
  if sf `Set.member` lockedFiles
    then do logInfo $ "Contention detected on source file " ++ (unSourceFile sf) ++ "; sleeping..."
            putMVar lox lockedFiles
            threadDelay 100000
            checkLock lox sf action
    else do putMVar lox $! (sf `Set.insert` lockedFiles)
            action
            modifyMVar_ lox (\s -> return $! sf `Set.delete` s)
  
getMTime :: SourceFile -> IO Time
getMTime sf = do
  result <- try $ getModificationTime (unSourceFile sf)
  case result of
    Right clockTime -> return $ floor (utcTimeToPOSIXSeconds . fromClockTime $ clockTime)
    Left e          -> do logInfo $ "Couldn't read mtime for file " ++ (unSourceFile sf) ++ ": " ++ (show (e :: IOException))
                          return 0  -- Most likely the file has been deleted.

doAnalyzeBuiltFile :: AnalysisChan -> DBChan -> DBChan -> Indexer -> CommandInfo -> IO ()
doAnalyzeBuiltFile aChan dbChan dbQueryChan indexer ci = do
    -- FIXME: Add an abstraction over this pattern.
    oldCI <- callLenChan dbQueryChan $ DBGetCommandInfo (ciSourceFile ci)
    mtime <- getMTime (ciSourceFile ci)
    when (isJust oldCI) $ logInfo $ "Deciding whether to analyze file " ++ (show . ciSourceFile $ ci) ++ " with last index time " ++ (show . ciLastIndexed $ ci)
    case oldCI of
      Just oldCI' | (ciLastIndexed oldCI') < mtime -> doAnalyze'
                  | otherwise                      -> do updateCommand dbChan ci
                                                         logInfo $ "Index is up-to-date for built file " ++ (show . ciSourceFile $ ci) ++ " (file mtime: " ++ (show mtime) ++ ")"
      _                                            -> doAnalyze'
  where
    doAnalyze' = do others <- otherFilesToReindex dbQueryChan ci
                    forM_ others $ \f -> writeLenChan aChan (AnalyzeBuiltFile f)
                    analyzeCode aChan dbChan indexer ci

doAnalyzeNotifiedFile :: AnalysisChan -> DBChan -> DBChan -> Indexer -> SourceFile -> IO ()
doAnalyzeNotifiedFile aChan dbChan dbQueryChan indexer sf = do
    oldCI <- callLenChan dbQueryChan $ DBGetCommandInfo sf
    case oldCI of
      Just oldCI' -> do mtime <- getMTime (ciSourceFile oldCI')
                        doAnalyzeSource' oldCI' mtime
      _           -> logInfo $ "Not indexing unknown file " ++ (show sf)
  where
    doAnalyzeSource' ci mt | (ciLastIndexed ci) < mt = do others <- otherFilesToReindex dbQueryChan ci
                                                          forM_ others $ \f -> writeLenChan aChan (AnalyzeBuiltFile f)
                                                          analyzeCode aChan dbChan indexer ci
                           | otherwise               = logInfo $ "Index is up-to-date for file " ++ (show sf)

-- If the source file associated with this CommandInfo has changed, what must
-- we reindex?
otherFilesToReindex :: DBChan -> CommandInfo -> IO [CommandInfo]
otherFilesToReindex dbQueryChan ci = callLenChan dbQueryChan $ DBGetIncluders (ciSourceFile ci)

updateCommand :: DBChan -> CommandInfo -> IO ()
updateCommand dbChan ci = writeLenChan dbChan (DBUpdateCommandInfo ci)

analyzeCode :: AnalysisChan -> DBChan -> Indexer -> CommandInfo -> IO ()
analyzeCode aChan dbChan indexer ci = do
    logInfo $ "Indexing " ++ (show sf)
    writeLenChan dbChan (DBResetInclusions sf)
    runResourceT (source $= conduitPut putReq =$= indexer =$= conduitGet getResp $$ process)
    updateCommand dbChan ci
  where
    sf = ciSourceFile ci
    source = yield (CR.Analyze ci) >> yield (CR.Shutdown)
    putReq :: Putter CR.ClangRequest
    putReq = put
    getResp = get :: Get CR.ClangResponse
    process = do
      liftIO $ logDebug "WAITING"
      resp <- await
      case resp of
        Just (CR.FoundDef di)       -> liftIO (writeLenChan dbChan (DBUpdateDefInfo di)) >> process
        Just (CR.FoundOverride ov)  -> liftIO (writeLenChan dbChan (DBUpdateOverride ov)) >> process
        Just (CR.FoundRef rf)       -> liftIO (writeLenChan dbChan (DBUpdateRef rf)) >> process
        Just (CR.FoundInclusion ic) -> handleInclusion ic >> process
        Just (CR.EndOfInclusions)   -> liftIO (writeLenChan dbChan (DBResetMetadata sf)) >> process
        Just (CR.EndOfAnalysis)     -> liftIO (logDebug "Done reading from clang process") >> return ()
        Nothing                     -> liftIO (logDebug "Clang process read failed") >> return ()
    handleInclusion ic = do
      -- FIXME: Awful.
      let cmd' = ciCommand ci
      let newCmd' = cmd' { cmdArguments = (incArgs . ciLanguage $ ci) ++ (cmdArguments cmd') }
      let newCI = ci { ciCommand = newCmd', ciLastIndexed = 0, ciSourceFile = icHeaderFile ic }
      liftIO (writeLenChan dbChan (DBUpdateInclusion ic))
      liftIO (writeLenChan aChan (AnalyzeBuiltFile newCI))
    incArgs CLanguage       = ["-x", "c"]
    incArgs CPPLanguage     = ["-x", "c++"]
    incArgs UnknownLanguage = []
