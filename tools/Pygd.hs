{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (Exception, throw)
import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS as FP
import GHC.Conc
import System.Directory
import System.FilePath.Posix
import System.FSNotify
import System.Path.NameManip

import Control.Concurrent.Chan.Counting
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
  ensureDB
  cf <- getConfiguration
  sfMVar <- newEmptyMVar
  port <- newEmptyMVar
  aChan <- newCountingChan
  dbChan <- newCountingChan
  logDebug "Launching database thread"
  dbThread <- asyncBound (runDatabaseManager dbChan)
  --let maxThreads = numCapabilities
  let maxThreads = 4 :: Int
  threads <- forM [1..maxThreads] $ \i -> do
    logDebug $ "Launching analysis thread #" ++ (show i)
    asyncBound (runAnalysisManager aChan dbChan)
  void $ race (runRPCServer cf port aChan dbChan) (withManager $ watch aChan dbChan sfMVar)
  forM_ threads $ \_ -> writeCountingChan aChan ShutdownAnalysis  -- Signifies end of data.
  forM_ (zip threads [1..numCapabilities]) $ \(thread, i) -> do
    ensureNoException =<< waitCatch thread
    logDebug $ "Termination of thread #" ++ (show i)
  writeCountingChan dbChan DBShutdown  -- Terminate the database thread.
  ensureNoException =<< waitCatch dbThread
  logDebug $ "Termination of database thread"

watch :: AnalysisChan -> DBChan -> MVar (Maybe CommandInfo) -> WatchManager -> IO ()
watch aChan dbChan sfMVar m = do
    curDir <- getCurrentDirectory
    logDebug $ "Started watching " ++ (show curDir) ++ "."
    watchTree m (FP.decodeString curDir) (const True) doEvent
    _ <- getLine
    logDebug "Stopped watching."
  where
    doEvent e = runReaderT (handleEvent e) (aChan, dbChan, sfMVar)

type EventReader a = ReaderT (AnalysisChan, DBChan, MVar (Maybe CommandInfo)) IO a

handleEvent :: Event -> EventReader ()
handleEvent (Added f t)    | isSource (FP.encodeString f) = handleSource f t
handleEvent (Modified f t) | isSource (FP.encodeString f) = handleSource f t
handleEvent (Removed f t)  | isSource (FP.encodeString f) = handleSource f t
handleEvent _                                             = return ()

handleSource :: FP.FilePath -> UTCTime -> EventReader ()
handleSource f t = do
  let file = FP.encodeString f
  liftIO $ logInfo $ (show f) ++ " was touched at " ++ (show t)
  exists <- liftIO $ doesFileExist file
  when exists (triggerAnalysis $ mkSourceFile file)

triggerAnalysis :: SourceFile -> EventReader ()
triggerAnalysis f = do
  (aChan, dbChan, _) <- ask
  time <- liftIO $ getPOSIXTime
  liftIO $ writeCountingChan aChan (AnalyzeSource f (floor time))

isSource :: FilePath -> Bool
isSource f = (hasSourceExtension f || hasHeaderExtension f) &&
             null (slice_path f `intersect` illegalPaths) &&
             not ("." `isPrefixOf` (takeFileName f))

illegalPaths :: [FilePath]
illegalPaths = [".git", ".hg", ".svn", "_darcs"]

ensureNoException :: Exception a => Either a b -> IO b
ensureNoException (Right v) = return v
ensureNoException (Left e)  = logError "Analysis thread threw an exception" >> throw e
