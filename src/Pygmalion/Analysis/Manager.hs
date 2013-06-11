{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Pygmalion.Analysis.Manager
( runAnalysisManager
, AnalysisRequest (..)
, AnalysisSource (..)
, AnalysisChan
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.DateTime
import Data.Time.Clock.POSIX
import qualified Data.Set as Set
import System.Directory
import System.Exit
import System.Process

import Control.Concurrent.Chan.Len
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Log

data AnalysisSource = FromBuild  CommandInfo
                    | FromNotify SourceFile

sfFromSource :: AnalysisSource -> SourceFile
sfFromSource (FromBuild  ci) = ciSourceFile ci
sfFromSource (FromNotify sf) = sf

data AnalysisRequest = Analyze AnalysisSource
                     | ShutdownAnalysis

type AnalysisChan = LenChan AnalysisRequest

data AnalysisContext = AnalysisContext
  { acPort         :: !Port
  , acAnalysisChan :: !AnalysisChan
  , acDBChan       :: !DBChan
  , acDBQueryChan  :: !DBChan
  }
type Analysis a = ReaderT AnalysisContext IO a

runAnalysisManager :: Port -> AnalysisChan -> DBChan -> DBChan -> MVar (Set.Set SourceFile) -> IO ()
runAnalysisManager port aChan dbChan dbQueryChan lox = go
  where
    go = {-# SCC "analysisThread" #-} do
         (!newCount, !req) <- readLenChan aChan
         logDebug $ "Analysis channel now has " ++ (show newCount) ++ " items waiting"
         case req of
             Analyze src -> do let ctx = AnalysisContext port aChan dbChan dbQueryChan
                               runReaderT (checkLock lox src) ctx >> go
             ShutdownAnalysis -> logInfo "Shutting down analysis thread"

checkLock :: MVar (Set.Set SourceFile) -> AnalysisSource -> Analysis ()
checkLock !lox !src = do
  let sf = sfFromSource src
  lockedFiles <- lift $ takeMVar lox
  if sf `Set.member` lockedFiles
    then do lift $ logInfo $ "Contention detected on source file "
                          ++ (unSourceFile sf) ++ "; sleeping..."
            lift $ putMVar lox lockedFiles
            lift $ threadDelay 100000
            checkLock lox src
    else do lift $ putMVar lox $! (sf `Set.insert` lockedFiles)
            analyzeIfDirty src
            lift $ modifyMVar_ lox (\s -> return $! sf `Set.delete` s)
  
getMTime :: SourceFile -> IO Time
getMTime sf = do
  result <- try $ getModificationTime (unSourceFile sf)
  case result of
    Right clockTime -> return . floor . utcTimeToPOSIXSeconds . fromClockTime $
                          clockTime
    Left e          -> do logInfo $ "Couldn't read mtime for file "
                                 ++ (unSourceFile sf) ++ ": "
                                 ++ (show (e :: IOException))
                          return 0  -- Most likely the file has been deleted.

analyzeIfDirty :: AnalysisSource -> Analysis ()
analyzeIfDirty src = do
  ctx <- ask
  let sf = sfFromSource src
  mayOldCI <- lift $ callLenChan (acDBQueryChan ctx) $ DBGetCommandInfo sf
  mtime <- lift $ getMTime sf
  case (src, mayOldCI) of
    (FromBuild ci, Just oldCI)
      | (ciLastIndexed oldCI) < mtime -> analyze ci
      | commandInfoChanged ci oldCI   -> analyze ci
      | otherwise                     -> ignoreUnchanged src mtime
    (FromBuild ci, Nothing)           -> analyze ci
    (FromNotify _, Just oldCI)
      | (ciLastIndexed oldCI) < mtime -> analyze oldCI
      | otherwise                     -> ignoreUnchanged src mtime
    (FromNotify _, Nothing)           -> ignoreUnknown src

commandInfoChanged :: CommandInfo -> CommandInfo -> Bool
commandInfoChanged a b | (ciWorkingPath a) /= (ciWorkingPath b) = True
                       | (ciCommand a)     /= (ciCommand b)     = True
                       | (ciArgs a)        /= (ciArgs b)        = True
                       | (ciLanguage a)    /= (ciLanguage b)    = True
                       | otherwise                              = False

analyze :: CommandInfo -> Analysis ()
analyze ci = do
  ctx <- ask
  others <- otherFilesToReindex ci
  forM_ others $ \f ->
    lift $ writeLenChan (acAnalysisChan ctx) (Analyze . FromBuild $ f)
  analyzeCode ci

ignoreUnchanged :: AnalysisSource -> Time -> Analysis ()
ignoreUnchanged src mtime = lift $ logInfo $ "Index is up-to-date for file "
                                          ++ (show . sfFromSource $ src)
                                          ++ " (file mtime: " ++ (show mtime) ++ ")"

ignoreUnknown :: AnalysisSource -> Analysis ()
ignoreUnknown src = lift $ logInfo $ "Not indexing unknown file "
                                  ++ (show . sfFromSource $ src)

-- If the source file associated with this CommandInfo has changed, what must
-- we reindex?
otherFilesToReindex :: CommandInfo -> Analysis [CommandInfo]
otherFilesToReindex ci = do
  ctx <- ask
  lift $ callLenChan (acDBQueryChan ctx) $ DBGetIncluders (ciSourceFile ci)

updateCommand :: CommandInfo -> Analysis ()
updateCommand ci = do
  ctx <- ask
  lift $ writeLenChan (acDBChan ctx) (DBUpdateCommandInfo ci)

analyzeCode :: CommandInfo -> Analysis ()
analyzeCode ci = do
    ctx <- ask
    let sf = ciSourceFile ci
    lift $ logInfo $ "Indexing " ++ (show sf)
    time <- lift getPOSIXTime
    lift $ writeLenChan (acDBChan ctx) (DBResetMetadata sf)
    (_, _, _, h) <- lift $ createProcess
                         (proc "pygclangindex" [show (acPort ctx), show ci])
    code <- lift $ waitForProcess h
    case code of
      ExitSuccess -> updateCommand $ ci { ciLastIndexed = floor time }
      _           -> do lift $ logInfo $ "Indexing process failed"
                        updateCommand $ ci { ciLastIndexed = 0 }
