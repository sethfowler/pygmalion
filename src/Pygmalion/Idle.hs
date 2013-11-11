{-# LANGUAGE BangPatterns #-}

module Pygmalion.Idle
( runIdleManager
, IdleChan
, IdleRequest (..)
) where

import Control.Concurrent.STM
import Control.Concurrent.Suspend.Lifted (sDelay)
import Control.Concurrent.Timer (oneShotTimer, stopTimer)
import Control.Monad (forever, liftM, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Index.Manager (clearLastIndexedCache, IndexStream (..))
import Pygmalion.Log

data IdleRequest = IdleBarrier (Response ())
                   deriving (Show)

type IdleChan = LenChan IdleRequest

runIdleManager :: Config -> IdleChan -> IndexStream -> [TVar Int] -> IO ()
runIdleManager cf idleChan idxStream vs = forever $ do
  -- Start the idle timer once activity stops.
  waitForAllEmpty idxStream vs
  logDebug "Starting the idle timer..."
  timer <- oneShotTimer (onIdle idleChan idxStream) (sDelay . idleDelay $ cf)

  -- Wait for activity and stop the idle timer.
  waitForAnyNonempty idxStream vs
  logDebug "Stopping the idle timer..."
  stopTimer timer
  
onIdle :: IdleChan -> IndexStream -> IO ()
onIdle idleChan idxStream = do
    logInfo "Returned to idle."
    atomically $ clearLastIndexedCache idxStream
    go
  where
    go = do (!newCount, !req) <- readLenChan idleChan
            case req of
              IdleBarrier !v -> doIdleBarrier v
            when (newCount > 0) go

doIdleBarrier :: Response () -> IO ()
doIdleBarrier v = do
  logInfo "Responding to idle barrier."
  sendResponse v =<< return ()

waitForAllEmpty :: MonadIO m => IndexStream -> [TVar Int] -> m ()
waitForAllEmpty idxStream vs = liftIO $ atomically $ do
  -- Check index stream.
  idxPending <- readTMVar (isPending idxStream)
  check (Map.null idxPending)
  idxCurrent <- readTMVar (isCurrent idxStream)
  check (Set.null idxCurrent)

  -- Check the other variables.
  varResults <- mapM (liftM (== 0) . readTVar) vs
  check (and varResults)

waitForAnyNonempty :: MonadIO m => IndexStream -> [TVar Int] -> m ()
waitForAnyNonempty idxStream vs = liftIO $ atomically $ do
  idxPending <- readTMVar (isPending idxStream)
  when (Map.null idxPending) $ do
    idxCurrent <- readTMVar (isCurrent idxStream)
    when (Set.null idxCurrent) $ do
      varResults <- mapM (liftM (/= 0) . readTVar) vs
      check (or varResults)
