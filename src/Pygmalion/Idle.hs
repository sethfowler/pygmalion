{-# LANGUAGE RankNTypes #-}

module Pygmalion.Idle
( runIdleManager
) where

import Control.Concurrent.STM
import Control.Concurrent.Suspend.Lifted (sDelay)
import Control.Concurrent.Timer (oneShotTimer, stopTimer)
import Control.Monad (forever, liftM)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Pygmalion.Config
import Pygmalion.Log

runIdleManager :: Config -> IO () -> [TVar Int] -> IO ()
runIdleManager cf act cs = forever $ do
    -- Start the idle timer once activity stops.
    waitForAllEmpty cs
    logDebug "Starting the idle timer..."
    timer <- oneShotTimer act (sDelay . idleDelay $ cf)

    -- Wait for activity and stop the idle timer.
    waitForAnyNonempty cs
    logDebug "Stopping the idle timer..."
    stopTimer timer
  
waitForAllEmpty :: MonadIO m => [TVar Int] -> m ()
waitForAllEmpty cs = liftIO $ atomically $ do
  results <- mapM (liftM (== 0) .readTVar) cs
  check (and results)

waitForAnyNonempty :: MonadIO m => [TVar Int] -> m ()
waitForAnyNonempty cs = liftIO $ atomically $ do
  results <- mapM (liftM (/= 0) .readTVar) cs
  check (or results)
