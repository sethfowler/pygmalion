{-# LANGUAGE BangPatterns #-}

module Pygmalion.Idle
( runIdleManager
, IdleChan
, IdleRequest (..)
) where

import Control.Concurrent.STM
import Control.Concurrent.Suspend.Lifted (sDelay)
import Control.Concurrent.Timer (oneShotTimer, stopTimer)
import Control.Monad (forever, liftM)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Log

data IdleRequest = IdleBarrier (Response ())
                   deriving (Show)

type IdleChan = LenChan IdleRequest

runIdleManager :: Config -> IdleChan -> [TVar Int] -> IO ()
runIdleManager cf idleChan vs = forever $ do
  -- Start the idle timer once activity stops.
  waitForAllEmpty vs
  logDebug "Starting the idle timer..."
  timer <- oneShotTimer (onIdle idleChan) (sDelay . idleDelay $ cf)

  -- Wait for activity and stop the idle timer.
  waitForAnyNonempty vs
  logDebug "Stopping the idle timer..."
  stopTimer timer
  
onIdle :: IdleChan -> IO ()
onIdle idleChan = do
    logInfo "Returned to idle."
    go
  where
    go = do (!newCount, !req) <- readLenChan idleChan
            case req of
              IdleBarrier !v -> doIdleBarrier v
            if newCount > 0 then go
                            else return ()

doIdleBarrier :: Response () -> IO ()
doIdleBarrier v = do
  logDebug $ "Responding to idle barrier"
  sendResponse v =<< return ()

waitForAllEmpty :: MonadIO m => [TVar Int] -> m ()
waitForAllEmpty vs = liftIO $ atomically $ do
  results <- mapM (liftM (== 0) . readTVar) vs
  check (and results)

waitForAnyNonempty :: MonadIO m => [TVar Int] -> m ()
waitForAnyNonempty vs = liftIO $ atomically $ do
  results <- mapM (liftM (/= 0) . readTVar) vs
  check (or results)
