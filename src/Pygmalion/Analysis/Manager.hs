{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Pygmalion.Analysis.Manager
( runAnalysisManager
, AnalysisRequest (..)
, AnalysisChan
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.DateTime
import Data.Maybe
import Data.Time.Clock.POSIX
import qualified Data.Set as Set
import System.Directory
import System.Exit
import System.Process

import Control.Concurrent.Chan.Len
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Log

data AnalysisRequest = AnalyzeBuiltFile CommandInfo
                     | AnalyzeNotifiedFile SourceFile
                     | ShutdownAnalysis
type AnalysisChan = LenChan AnalysisRequest

runAnalysisManager :: Port -> AnalysisChan -> DBChan -> DBChan -> MVar (Set.Set SourceFile) -> IO ()
runAnalysisManager port aChan dbChan dbQueryChan lox = go
  where
    go = {-# SCC "analysisThread" #-} do
         (!newCount, !req) <- readLenChan aChan
         logDebug $ "Analysis channel now has " ++ (show newCount) ++ " items waiting"
         case req of
             AnalyzeBuiltFile !ci    -> checkLock lox (ciSourceFile ci) (doAnalyzeBuiltFile port aChan dbChan dbQueryChan ci) >> go
             AnalyzeNotifiedFile !sf -> checkLock lox sf (doAnalyzeNotifiedFile port aChan dbChan dbQueryChan sf) >> go
             ShutdownAnalysis        -> logInfo "Shutting down analysis thread"

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

doAnalyzeBuiltFile :: Port -> AnalysisChan -> DBChan -> DBChan -> CommandInfo -> IO ()
doAnalyzeBuiltFile port aChan dbChan dbQueryChan ci = do
    -- FIXME: Add an abstraction over this pattern.
    oldCI <- callLenChan dbQueryChan $ DBGetCommandInfo (ciSourceFile ci)
    mtime <- getMTime (ciSourceFile ci)
    when (isJust oldCI) $
      logInfo $ "Index built? File: " ++ (show . ciSourceFile $ ci) ++
                " Last index: " ++ (show . ciLastIndexed . fromJust $ oldCI) ++
                " mtime: " ++ (show mtime)
    case oldCI of
      Just oldCI' | (ciLastIndexed oldCI') < mtime -> doAnalyze'
                  | otherwise                      -> do -- Update command but not index time.
                                                         updateCommand dbChan oldCI'
                                                         logInfo $ "Index is up-to-date for built file " ++ (show . ciSourceFile $ ci) ++ " (file mtime: " ++ (show mtime) ++ ")"
      _                                            -> doAnalyze'
  where
    doAnalyze' = do others <- otherFilesToReindex dbQueryChan ci
                    forM_ others $ \f -> writeLenChan aChan (AnalyzeBuiltFile f)
                    analyzeCode port dbChan ci

doAnalyzeNotifiedFile :: Port -> AnalysisChan -> DBChan -> DBChan -> SourceFile -> IO ()
doAnalyzeNotifiedFile port aChan dbChan dbQueryChan sf = do
    oldCI <- callLenChan dbQueryChan $ DBGetCommandInfo sf
    case oldCI of
      Just oldCI' -> do mtime <- getMTime (ciSourceFile oldCI')
                        doAnalyzeSource' oldCI' mtime
      _           -> logInfo $ "Not indexing unknown file " ++ (show sf)
  where
    doAnalyzeSource' ci mt | (ciLastIndexed ci) < mt = do others <- otherFilesToReindex dbQueryChan ci
                                                          forM_ others $ \f -> writeLenChan aChan (AnalyzeBuiltFile f)
                                                          analyzeCode port dbChan ci
                           | otherwise               = logInfo $ "Index is up-to-date for file " ++ (show sf)

-- If the source file associated with this CommandInfo has changed, what must
-- we reindex?
otherFilesToReindex :: DBChan -> CommandInfo -> IO [CommandInfo]
otherFilesToReindex dbQueryChan ci = callLenChan dbQueryChan $ DBGetIncluders (ciSourceFile ci)

updateCommand :: DBChan -> CommandInfo -> IO ()
updateCommand dbChan ci = writeLenChan dbChan (DBUpdateCommandInfo ci)

analyzeCode :: Port -> DBChan -> CommandInfo -> IO ()
analyzeCode port dbChan ci = do
    logInfo $ "Indexing " ++ (show . ciSourceFile $ ci)
    time <- getPOSIXTime
    writeLenChan dbChan (DBResetMetadata . ciSourceFile $ ci)
    (_, _, _, h) <- createProcess (proc "pygclangindex" [show port, show ci])
    code <- waitForProcess h
    case code of
      ExitSuccess -> updateCommand dbChan $ ci { ciLastIndexed = floor time }
      _           -> do logInfo $ "Indexing process failed"
                        updateCommand dbChan $ ci { ciLastIndexed = 0 }
