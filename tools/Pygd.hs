{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (Exception, throw)
import Control.Monad
import Data.List
import qualified Filesystem.Path.CurrentOS as FP
import GHC.Conc
import System.Directory
import System.FilePath.Posix
import System.FSNotify
import System.Path.NameManip
import System.Posix.Process

import Control.Concurrent.Chan.Len
import Pygmalion.Analysis.Extension
import Pygmalion.Analysis.Manager
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Log
import Pygmalion.RPC.Server

main :: IO ()
main = do
  initLogger DEBUG -- Need to make this configurable.
  nice 5
  ensureDB
  cf <- getConfiguration
  stopWatching <- newEmptyMVar
  port <- newEmptyMVar
  aChan <- newLenChan
  dbChan <- newLenChan
  dbQueryChan <- newLenChan
  logDebug "Launching database thread"
  dbThread <- asyncBound (runDatabaseManager dbChan dbQueryChan)
  --let maxThreads = numCapabilities
  let maxThreads = 4 :: Int
  threads <- forM [1..maxThreads] $ \i -> do
    logDebug $ "Launching analysis thread #" ++ (show i)
    asyncBound (runAnalysisManager aChan dbChan dbQueryChan)
  rpcThread <- async (runRPCServer cf port aChan dbQueryChan)
  watchThread <- async (doWatch aChan stopWatching)
  _ <- getLine
  cancel rpcThread
  --logDebug "Just terminated RPC thread."
  --_ <- getLine
  putMVar stopWatching ()
  wait watchThread
  --logDebug "Just terminated watch thread."
  --_ <- getLine
  forM_ threads $ \_ -> writeLenChan aChan ShutdownAnalysis  -- Signifies end of data.
  forM_ (zip threads [1..numCapabilities]) $ \(thread, i) -> do
    ensureNoException =<< waitCatch thread
    logDebug $ "Termination of thread #" ++ (show i)
  writeLenChan dbChan DBShutdown  -- Terminate the database thread.
  ensureNoException =<< waitCatch dbThread
  logDebug $ "Termination of database thread"

doWatch :: AnalysisChan -> MVar () -> IO ()
doWatch aChan stopWatching = do
  -- Restart every 10 minutes until a better fix is found. =(
  _ <- race (withManager $ watch aChan stopWatching) (threadDelay $ 10 * 60 * 1000000)
  shouldStop <- tryTakeMVar stopWatching
  case shouldStop of
    Just _  -> return ()
    Nothing -> doWatch aChan stopWatching

watch :: AnalysisChan -> MVar () -> WatchManager -> IO ()
watch aChan stopWatching m = do
    curDir <- getCurrentDirectory
    logDebug $ "Started watching " ++ (show curDir) ++ "."
    watchTree m (FP.decodeString curDir) (checkEvent) (handleEvent aChan)
    readMVar stopWatching

checkEvent :: Event -> Bool
checkEvent (Added f _)    = isSource $! FP.encodeString $! f
checkEvent (Modified f _) = isSource $! FP.encodeString $! f
checkEvent (Removed f _)  = isSource $! FP.encodeString $! f

handleEvent :: AnalysisChan -> Event -> IO ()
handleEvent aChan (Added f _)    = handleSource aChan f
handleEvent aChan (Modified f _) = handleSource aChan f
handleEvent aChan (Removed f _)  = handleSource aChan f

handleSource :: AnalysisChan -> FP.FilePath -> IO ()
handleSource aChan f = do
  let file = FP.encodeString f
  fileExists <- doesFileExist file
  when (isSource file && fileExists) $ do
    writeLenChan aChan $ AnalyzeSource (mkSourceFile file)

isSource :: FilePath -> Bool
isSource f = (hasSourceExtension f || hasHeaderExtension f) &&
             null (slice_path f `intersect` illegalPaths) &&
             not ("." `isPrefixOf` (takeFileName f))

illegalPaths :: [FilePath]
illegalPaths = [".git", ".hg", ".svn", "_darcs"]

ensureNoException :: Exception a => Either a b -> IO b
ensureNoException (Right v) = return v
ensureNoException (Left e)  = do logError $ "Thread threw an exception: " ++ (show e)
                                 throw e
