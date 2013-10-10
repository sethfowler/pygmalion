{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Pygmalion.Index.Manager
( runIndexManager
, IndexRequest (..)
, IndexSource (..)
, IndexChan
, IndexLockSet
, mkIndexLockSet
, indexLockSetCounter
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Crypto.Hash.SHA1
import qualified Data.ByteString as B
import Data.Time.Clock.POSIX
import qualified Data.Set as Set
import System.Directory
import System.Exit
import System.Process

import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Log

data IndexSource = FromBuild  CommandInfo
                 | FromNotify SourceFile
                 deriving (Show)

sfFromSource :: IndexSource -> SourceFile
sfFromSource (FromBuild  ci) = ciSourceFile ci
sfFromSource (FromNotify sf) = sf

data IndexRequest = Index IndexSource
                  | ShutdownIndexer
                  deriving (Show)

type IndexChan = LenChan IndexRequest

data IndexContext = IndexContext
  { acPort         :: !Port
  , acIndexer      :: !String
  , acIndexChan    :: !IndexChan
  , acDBChan       :: !DBChan
  , acDBQueryChan  :: !DBChan
  }
type Indexer a = ReaderT IndexContext IO a

data IndexLockSet = IndexLockSet
  { lsSet     :: MVar (Set.Set SourceFile)
  , lsCounter :: TVar Int
  }

mkIndexLockSet :: IO IndexLockSet
mkIndexLockSet = do
  set     <- newMVar Set.empty
  counter <- newTVarIO 0
  return $! IndexLockSet set counter

takeIndexLockSet :: IndexLockSet -> Indexer (Set.Set SourceFile)
takeIndexLockSet ils = lift $ takeMVar (lsSet ils)

putIndexLockSet :: IndexLockSet -> Set.Set SourceFile -> Indexer ()
putIndexLockSet ils !set = do
  lift $ atomically $ writeTVar (lsCounter ils) (Set.size set)
  lift $ putMVar (lsSet ils) set

modifyIndexLockSet :: IndexLockSet -> (Set.Set SourceFile -> Indexer (Set.Set SourceFile))
                                   -> Indexer ()
modifyIndexLockSet ils f = do
  set <- takeIndexLockSet ils
  updatedSet <- f set
  putIndexLockSet ils updatedSet

indexLockSetCounter :: IndexLockSet -> TVar Int
indexLockSetCounter = lsCounter

runIndexManager :: Config -> IndexChan -> DBChan -> DBChan -> IndexLockSet -> IO ()
runIndexManager cf iChan dbChan dbQueryChan ils = go
  where
    ctx = IndexContext (ifPort cf) (idxCmd cf) iChan dbChan dbQueryChan
    go = {-# SCC "indexThread" #-} do
         (!newCount, !req) <- readLenChan iChan
         logDebug $ "Index request: " ++ show req
         logDebug $ "Index channel now has " ++ show newCount ++ " items waiting"
         case req of
             Index src       -> runReaderT (checkLock ils src) ctx >> go
             ShutdownIndexer -> logInfo "Shutting down analysis thread"

checkLock :: IndexLockSet -> IndexSource -> Indexer ()
checkLock !ils !src = do
  let sf = sfFromSource src
  lockedFiles <- takeIndexLockSet ils
  if sf `Set.member` lockedFiles
    then do logInfo $ "Contention detected on source file "
                   ++ unSourceFile sf ++ "; sleeping..."
            putIndexLockSet ils lockedFiles
            lift $ threadDelay 10000  -- Keep churn under control.
            ctx <- ask
            writeLenChan (acIndexChan ctx) (Index src)
    else do putIndexLockSet ils (sf `Set.insert` lockedFiles)
            analyzeIfDirty src
            modifyIndexLockSet ils (\s -> return $! sf `Set.delete` s)
  
getMTime :: SourceFile -> Indexer (Maybe Time)
getMTime sf = lift $ do
  result <- try $ getModificationTime (unSourceFile sf)
  case result of
    Right clockTime -> return . Just . floor . utcTimeToPOSIXSeconds $ clockTime
    Left e          -> do logInfo $ "Couldn't read mtime for file "
                                 ++ unSourceFile sf ++ ": "
                                 ++ show (e :: IOException)
                          return Nothing  -- Most likely the file has been deleted.

getSHA :: SourceFile -> Indexer B.ByteString
getSHA sf = lift $ do
  result <- try $ B.readFile (unSourceFile sf)
  case result of
    Right file -> return $ hash file
    Left e     -> do logInfo $ "Couldn't read and hash file "
                            ++ unSourceFile sf ++ ": "
                            ++ show (e :: IOException)
                     return B.empty -- Most likely the file has been deleted.

analyzeIfDirty :: IndexSource -> Indexer ()
analyzeIfDirty src = do
  ctx <- ask
  let sf = sfFromSource src
  mayOldCI <- callLenChan (acDBQueryChan ctx) $ DBGetCommandInfo sf
  mayMTime <- getMTime sf

  case (src, mayOldCI, mayMTime) of
    (_, _, Nothing)                   -> ignoreUnreadable src
    (FromBuild ci, Just oldCI, Just mtime)
      | commandInfoChanged ci oldCI   -> analyze ci
      | ciLastIndexed oldCI < mtime   -> analyzeIfSHADirty ci oldCI
      | otherwise                     -> ignoreUnchanged src mtime
    (FromBuild ci, Nothing, _)        -> analyze ci
    (FromNotify _, Just oldCI, Just mtime)
      | ciLastIndexed oldCI < mtime   -> analyzeIfSHADirty oldCI oldCI
      | otherwise                     -> ignoreUnchanged src mtime
    (FromNotify _, Nothing, _)        -> ignoreUnknown src

commandInfoChanged :: CommandInfo -> CommandInfo -> Bool
commandInfoChanged a b | ciWorkingPath a /= ciWorkingPath b = True
                       | ciCommand a     /= ciCommand b     = True
                       | ciArgs a        /= ciArgs b        = True
                       | ciLanguage a    /= ciLanguage b    = True
                       | otherwise                              = False

analyzeIfSHADirty :: CommandInfo -> CommandInfo -> Indexer ()
analyzeIfSHADirty ci oldCI = do
  -- Note that it's ok if ci and oldCI are the same. We just want to be able to
  -- pass on the new version to |analyze| if they're different.
  sha <- getSHA (ciSourceFile ci)
  if sha /= ciSHA oldCI then analyze $ ci { ciSHA = sha }
                        else shaUnchanged ci

shaUnchanged :: CommandInfo -> Indexer ()
shaUnchanged ci = do
  -- Update the last index time so we don't waste time computing the SHA again.
  logInfo $ "Index is up-to-date for file "
         ++ (show . ciSourceFile $ ci)
         ++ " (SHA1 digest is unchanged)"
  time <- lift getPOSIXTime
  updateCommand $ ci { ciLastIndexed = floor time }

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
                                   ++ " (file mtime: " ++ show mtime ++ ")"

ignoreUnknown :: IndexSource -> Indexer ()
ignoreUnknown src = logInfo $ "Not indexing unknown file "
                           ++ (show . sfFromSource $ src)

ignoreUnreadable :: IndexSource -> Indexer ()
ignoreUnreadable src = logInfo $ "Not indexing unreadable file "
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

-- If indexing failed, we want to be sure to reattempt indexing on the file next
-- time we have an opportunity. We make sure this happens by invalidating all of
-- the metadata that might ordinarily cause us to skip indexing the file.
invalidateCommand :: CommandInfo -> CommandInfo
invalidateCommand ci = ci { ciLastIndexed = 0
                          , ciSHA = B.empty
                          }

analyzeCode :: CommandInfo -> Indexer ()
analyzeCode ci = do
    ctx <- ask
    let sf = ciSourceFile ci
    logInfo $ "Indexing " ++ show sf
    time <- lift getPOSIXTime
    writeLenChan (acDBChan ctx) (DBResetMetadata sf)
    (_, _, _, h) <- lift $ createProcess
                         (proc (acIndexer ctx) [show (acPort ctx), show ci])
    code <- lift $ waitForProcess h
    case code of
      ExitSuccess -> updateCommand $ ci { ciLastIndexed = floor time }
      _           -> do logInfo "Indexing process failed"
                        updateCommand (invalidateCommand ci)
