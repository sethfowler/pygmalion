{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (Exception, fromException, toException)
import Control.Monad
import Data.List
import qualified Filesystem.Path.CurrentOS as FP
import GHC.Conc
import System.Directory
import System.FilePath.Posix
import System.FSNotify
import System.Path.NameManip
import qualified System.Remote.Monitoring as EKG

import Control.Concurrent.Chan.Len
import Pygmalion.Index.Extension
import Pygmalion.Index.Manager
import Pygmalion.Index.Request
import Pygmalion.Index.Stream
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Database.Request
import Pygmalion.File
import Pygmalion.Log
import Pygmalion.Idle
import Pygmalion.Metadata
import Pygmalion.RPC.Server

main :: IO ()
main = do
  -- Initialize.
  cf <- getConfiguration
  initLogger (logLevel cf)
  ekg <- EKG.forkServer "localhost" 8000

  -- Initialize the database and file metadata.
  ensureStagingDB
  ensureDB
  logInfo "Reading file metadata from database..."
  metadata <- readMetadata
  logInfo "Checking file metadata consistency..."
  case checkMetadata metadata of
    []   ->    logInfo "File metadata is consistent."
    errs -> do logWarn "File metadata is inconsistent:"
               mapM_ logWarn errs

  -- Create communication channels.
  stopWatching <- newEmptyMVar
  dbUpdateChan <- newDBUpdateChan
  dbQueryChan <- newLenChan
  idleChan <- newLenChan
  idxStream <- mkIndexStream metadata

  -- Launch threads.
  logDebug "Launching idle thread"
  idleThread <- asyncBound $ runIdleManager cf idleChan idxStream dbUpdateChan dbQueryChan
  logDebug "Launching database thread"
  dbThread <- asyncBound (runDatabaseManager dbUpdateChan dbQueryChan ekg)
  let maxThreads = case idxThreads cf of
                     0 -> numCapabilities
                     n -> n
  indexThreads <- forM [1..maxThreads] $ \i -> do
    logDebug $ "Launching indexing thread #" ++ show i
    asyncBound (runIndexManager cf dbUpdateChan idxStream)
  rpcThread <- async (runRPCServer cf idxStream dbUpdateChan dbQueryChan idleChan)
  watchThread <- async (doWatch idxStream stopWatching)

  -- Wait for something to terminate.
  void $ waitAnyCatch ([dbThread, rpcThread, watchThread] ++ indexThreads)

  -- Shut down RPC server.
  rpcResult <- poll rpcThread
  case rpcResult of
    Just r  -> ensureCleanExit r
    Nothing -> cancel rpcThread
  logDebug "Termination of RPC Server thread."

  -- Shut down idle thread.
  idleResult <- poll idleThread
  case idleResult of
    Just r  -> ensureCleanExit r
    Nothing -> cancel idleThread
  logDebug "Termination of idle thread."

  -- Shut down the other threads.
  putMVar stopWatching ()
  ensureNoException =<< waitCatch watchThread
  logDebug "Terminated watch thread."
  atomically $ shutdownIndexStream idxStream
  forM_ (zip indexThreads [1..numCapabilities]) $ \(thread, i) -> do
    ensureNoException =<< waitCatch thread
    logDebug $ "Termination of thread #" ++ show i
  writeLenChan dbQueryChan DBShutdown  -- Terminate the database thread.
  ensureNoException =<< waitCatch dbThread
  logDebug "Termination of database thread"

doWatch :: IndexStream -> MVar () -> IO ()
doWatch idxStream stopWatching = do
  -- Restart every 10 minutes until a better fix is found. =(
  _ <- race (withManager $ watch idxStream stopWatching) (threadDelay $ 10 * 60 * 1000000)
  shouldStop <- tryTakeMVar stopWatching
  case shouldStop of
    Just _  -> return ()
    Nothing -> doWatch idxStream stopWatching

watch :: IndexStream -> MVar () -> WatchManager -> IO ()
watch idxStream stopWatching m = do
    curDir <- getCurrentDirectory
    logDebug $ "Started watching " ++ show curDir ++ "."
    watchTree m (FP.decodeString curDir) checkEvent (handleEvent idxStream)
    readMVar stopWatching

checkEvent :: Event -> Bool
checkEvent (Added f _)    = isSource $! FP.encodeString $! f
checkEvent (Modified f _) = isSource $! FP.encodeString $! f
checkEvent (Removed f _)  = isSource $! FP.encodeString $! f

handleEvent :: IndexStream -> Event -> IO ()
handleEvent idxStream (Added f _)    = handleSource idxStream f
handleEvent idxStream (Modified f _) = handleSource idxStream f
handleEvent idxStream (Removed f _)  = handleSource idxStream f

handleSource :: IndexStream -> FP.FilePath -> IO ()
handleSource idxStream f = do
  let file = FP.encodeString f
  fileExists <- doesFileExist file
  when (isSource file && fileExists) $ do
    let sf = mkSourceFile file
    mayMTime <- getMTime sf
    case mayMTime of
      Just mtime -> atomically $ addPendingIndex idxStream
                               $ indexRequestForUpdate sf mtime
      Nothing    -> return ()  -- Couldn't stat the file.

isSource :: FilePath -> Bool
isSource f = case extensionKind f of
               UnknownExtension -> False
               _                -> null (slice_path f `intersect` illegalPaths) &&
                                   not ("." `isPrefixOf` takeFileName f)

illegalPaths :: [FilePath]
illegalPaths = [".git", ".hg", ".svn", "_darcs"]

ensureNoException :: Exception a => Either a b -> IO ()
ensureNoException (Right _) = return ()
ensureNoException (Left e)  = logError $ "Thread threw an exception: " ++ show e

ensureCleanExit :: Exception a => Either a b -> IO ()
ensureCleanExit (Right _) = return ()
ensureCleanExit (Left e) = do
  let mayRPCServerExit = (fromException . toException $ e) :: Maybe RPCServerExit
  case mayRPCServerExit of
    Just _  -> return ()
    Nothing -> logError $ "RPC server threw an exception: " ++ show e
