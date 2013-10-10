{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (Exception, fromException, toException)
import Control.Monad
import Data.List
import qualified Filesystem.Path.CurrentOS as FP
import GHC.Conc
import System.Directory
import System.FilePath.Posix
import System.FSNotify
import System.Path.NameManip

import Control.Concurrent.Chan.Len
import Pygmalion.Index.Extension
import Pygmalion.Index.Manager
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Log
import Pygmalion.Idle
import Pygmalion.RPC.Server

main :: IO ()
main = do
  -- Initialize.
  cf <- getConfiguration
  initLogger (logLevel cf)
  ensureDB
  stopWatching <- newEmptyMVar
  aChan <- newLenChan
  dbChan <- newLenChan
  dbQueryChan <- newLenChan
  fileLox <- mkIndexLockSet

  -- Launch threads.
  logDebug "Launching idle thread"
  idleThread <- asyncBound (runIdleManager cf
                                           onIdle
                                           [lenChanCounter aChan,
                                            lenChanCounter dbChan,
                                            lenChanCounter dbQueryChan,
                                            indexLockSetCounter fileLox])
  logDebug "Launching database thread"
  dbThread <- asyncBound (runDatabaseManager dbChan dbQueryChan)
  let maxThreads = case idxThreads cf of
                     0 -> numCapabilities
                     n -> n
  indexThreads <- forM [1..maxThreads] $ \i -> do
    logDebug $ "Launching indexing thread #" ++ show i
    asyncBound (runIndexManager cf aChan dbChan dbQueryChan fileLox)
  rpcThread <- async (runRPCServer cf aChan dbChan dbQueryChan)
  watchThread <- async (doWatch aChan stopWatching)

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
  forM_ indexThreads $ \_ -> writeLenChan aChan ShutdownIndexer  -- Signifies end of data.
  forM_ (zip indexThreads [1..numCapabilities]) $ \(thread, i) -> do
    ensureNoException =<< waitCatch thread
    logDebug $ "Termination of thread #" ++ show i
  writeLenChan dbChan DBShutdown  -- Terminate the database thread.
  ensureNoException =<< waitCatch dbThread
  logDebug "Termination of database thread"

onIdle :: IO ()
onIdle = do
  logInfo "Returned to idle."

doWatch :: IndexChan -> MVar () -> IO ()
doWatch aChan stopWatching = do
  -- Restart every 10 minutes until a better fix is found. =(
  _ <- race (withManager $ watch aChan stopWatching) (threadDelay $ 10 * 60 * 1000000)
  shouldStop <- tryTakeMVar stopWatching
  case shouldStop of
    Just _  -> return ()
    Nothing -> doWatch aChan stopWatching

watch :: IndexChan -> MVar () -> WatchManager -> IO ()
watch aChan stopWatching m = do
    curDir <- getCurrentDirectory
    logDebug $ "Started watching " ++ show curDir ++ "."
    watchTree m (FP.decodeString curDir) checkEvent (handleEvent aChan)
    readMVar stopWatching

checkEvent :: Event -> Bool
checkEvent (Added f _)    = isSource $! FP.encodeString $! f
checkEvent (Modified f _) = isSource $! FP.encodeString $! f
checkEvent (Removed f _)  = isSource $! FP.encodeString $! f

handleEvent :: IndexChan -> Event -> IO ()
handleEvent aChan (Added f _)    = handleSource aChan f
handleEvent aChan (Modified f _) = handleSource aChan f
handleEvent aChan (Removed f _)  = handleSource aChan f

handleSource :: IndexChan -> FP.FilePath -> IO ()
handleSource aChan f = do
  let file = FP.encodeString f
  fileExists <- doesFileExist file
  when (isSource file && fileExists) $
    writeLenChan aChan $ Index . FromNotify . mkSourceFile $ file

isSource :: FilePath -> Bool
isSource f = (hasSourceExtension f || hasHeaderExtension f) &&
             null (slice_path f `intersect` illegalPaths) &&
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
