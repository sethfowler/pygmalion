{-# LANGUAGE BangPatterns #-}

module Pygmalion.Idle
( runIdleManager
, IdleChan
, IdleRequest (..)
) where

import Control.Concurrent.STM
import Control.Concurrent.Suspend.Lifted (sDelay)
import Control.Concurrent.Timer (oneShotTimer, stopTimer)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.IntMap as Map
import qualified Data.IntSet as Set

import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Index.Stream (clearLastIndexedCache, IndexStream (..))
import Pygmalion.Log

data IdleRequest = IdleBarrier (Response ())
                   deriving (Show)

type IdleChan = LenChan IdleRequest

runIdleManager :: Config -> IdleChan -> IndexStream -> [LenChan a] -> IO ()
runIdleManager cf idleChan idxStream cs = forever $ do
  -- Start the idle timer once activity stops.
  waitForAllEmpty idxStream cs
  logDebug "Starting the idle timer..."
  timer <- oneShotTimer (onIdle idleChan idxStream) (sDelay . idleDelay $ cf)

  -- Wait for activity and stop the idle timer.
  waitForAnyNonempty idxStream cs
  logDebug "Stopping the idle timer..."
  stopTimer timer
  
onIdle :: IdleChan -> IndexStream -> IO ()
onIdle idleChan idxStream = do
    logInfo "Returned to idle."
    atomically $ clearLastIndexedCache idxStream
    go
  where
    go = do !req <- readLenChan idleChan
            case req of
              IdleBarrier !v -> doIdleBarrier v
            isEmpty <- isEmptyLenChan idleChan
            unless isEmpty go

doIdleBarrier :: Response () -> IO ()
doIdleBarrier v = do
  logInfo "Responding to idle barrier."
  sendResponse v =<< return ()

waitForAllEmpty :: MonadIO m => IndexStream -> [LenChan a] -> m ()
waitForAllEmpty idxStream cs = liftIO $ atomically $ do
  -- Check index stream.
  idxPending <- readTMVar (isPending idxStream)
  check (Map.null idxPending)
  idxCurrent <- readTMVar (isCurrent idxStream)
  check (Set.null idxCurrent)

  -- Check the channels.
  chanResults <- mapM isEmptyLenChan' cs
  check (and chanResults)

waitForAnyNonempty :: MonadIO m => IndexStream -> [LenChan a] -> m ()
waitForAnyNonempty idxStream cs = liftIO $ atomically $ do
  idxPending <- readTMVar (isPending idxStream)
  when (Map.null idxPending) $ do
    idxCurrent <- readTMVar (isCurrent idxStream)
    when (Set.null idxCurrent) $ do
      chanResults <- mapM isEmptyLenChan' cs
      check $ not (and chanResults)
