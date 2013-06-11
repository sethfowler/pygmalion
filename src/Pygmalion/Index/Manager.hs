{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Pygmalion.Index.Manager
( runIndexManager
, IndexRequest (..)
, IndexSource (..)
, IndexChan
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

data IndexSource = FromBuild  CommandInfo
                 | FromNotify SourceFile

sfFromSource :: IndexSource -> SourceFile
sfFromSource (FromBuild  ci) = ciSourceFile ci
sfFromSource (FromNotify sf) = sf

data IndexRequest = Index IndexSource
                  | ShutdownIndexer

type IndexChan = LenChan IndexRequest

data IndexContext = IndexContext
  { acPort         :: !Port
  , acIndexChan    :: !IndexChan
  , acDBChan       :: !DBChan
  , acDBQueryChan  :: !DBChan
  }
type Indexer a = ReaderT IndexContext IO a

runIndexManager :: Port -> IndexChan -> DBChan -> DBChan -> MVar (Set.Set SourceFile) -> IO ()
runIndexManager port aChan dbChan dbQueryChan lox = go
  where
    go = {-# SCC "indexThread" #-} do
         (!newCount, !req) <- readLenChan aChan
         logDebug $ "Index channel now has " ++ (show newCount) ++ " items waiting"
         case req of
             Index src    -> do let ctx = IndexContext port aChan dbChan dbQueryChan
                                runReaderT (checkLock lox src) ctx >> go
             ShutdownIndexer -> logInfo "Shutting down analysis thread"

checkLock :: MVar (Set.Set SourceFile) -> IndexSource -> Indexer ()
checkLock !lox !src = do
  let sf = sfFromSource src
  lockedFiles <- lift $ takeMVar lox
  if sf `Set.member` lockedFiles
    then do logInfo $ "Contention detected on source file "
                   ++ (unSourceFile sf) ++ "; sleeping..."
            lift $ putMVar lox lockedFiles
            lift $ threadDelay 100000
            checkLock lox src
    else do lift $ putMVar lox $! (sf `Set.insert` lockedFiles)
            analyzeIfDirty src
            lift $ modifyMVar_ lox (\s -> return $! sf `Set.delete` s)
  
getMTime :: SourceFile -> Indexer Time
getMTime sf = liftIO $ do
  result <- try $ getModificationTime (unSourceFile sf)
  case result of
    Right clockTime -> return . floor . utcTimeToPOSIXSeconds . fromClockTime $
                          clockTime
    Left e          -> do logInfo $ "Couldn't read mtime for file "
                                 ++ (unSourceFile sf) ++ ": "
                                 ++ (show (e :: IOException))
                          return 0  -- Most likely the file has been deleted.

analyzeIfDirty :: IndexSource -> Indexer ()
analyzeIfDirty src = do
  ctx <- ask
  let sf = sfFromSource src
  mayOldCI <- callLenChan (acDBQueryChan ctx) $ DBGetCommandInfo sf
  mtime <- getMTime sf
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

analyze :: CommandInfo -> Indexer ()
analyze ci = do
  ctx <- ask
  others <- otherFilesToReindex ci
  forM_ others $ \f ->
    writeLenChan (acIndexChan ctx) (Index . FromBuild $ f)
  analyzeCode ci

ignoreUnchanged :: IndexSource -> Time -> Indexer ()
ignoreUnchanged src mtime = logInfo $ "Index is up-to-date for file "
                                   ++ (show . sfFromSource $ src)
                                   ++ " (file mtime: " ++ (show mtime) ++ ")"

ignoreUnknown :: IndexSource -> Indexer ()
ignoreUnknown src = logInfo $ "Not indexing unknown file "
                           ++ (show . sfFromSource $ src)

-- If the source file associated with this CommandInfo has changed, what must
-- we reindex?
otherFilesToReindex :: CommandInfo -> Indexer [CommandInfo]
otherFilesToReindex ci = do
  ctx <- ask
  callLenChan (acDBQueryChan ctx) $ DBGetIncluders (ciSourceFile ci)

updateCommand :: CommandInfo -> Indexer ()
updateCommand ci = do
  ctx <- ask
  writeLenChan (acDBChan ctx) (DBUpdateCommandInfo ci)

analyzeCode :: CommandInfo -> Indexer ()
analyzeCode ci = do
    ctx <- ask
    let sf = ciSourceFile ci
    logInfo $ "Indexing " ++ (show sf)
    time <- lift getPOSIXTime
    writeLenChan (acDBChan ctx) (DBResetMetadata sf)
    (_, _, _, h) <- lift $ createProcess
                         (proc "pygclangindex" [show (acPort ctx), show ci])
    code <- lift $ waitForProcess h
    case code of
      ExitSuccess -> updateCommand $ ci { ciLastIndexed = floor time }
      _           -> do logInfo $ "Indexing process failed"
                        updateCommand $ ci { ciLastIndexed = 0 }
