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
import Pygmalion.Database.Request
import Pygmalion.Index.Stream (IndexStream (..))
import Pygmalion.Log

data IdleRequest = IdleBarrier (Response ())
                   deriving (Show)

type IdleChan = LenChan IdleRequest

runIdleManager :: Config -> IdleChan -> IndexStream -> DBUpdateChan -> DBQueryChan -> IO ()
runIdleManager cf idleChan idxStream dbUpdateChan dbQueryChan = forever $ do
  -- Start the idle timer once activity stops.
  waitForAllEmpty idxStream dbUpdateChan dbQueryChan
  logDebug "Starting the idle timer..."
  timer <- oneShotTimer (onIdle idleChan) (sDelay . idleDelay $ cf)

  -- Wait for activity and stop the idle timer.
  waitForAnyNonempty idxStream dbUpdateChan dbQueryChan
  logDebug "Stopping the idle timer..."
  stopTimer timer
  
onIdle :: IdleChan -> IO ()
onIdle idleChan = do
    logInfo "Returned to idle."
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

waitForAllEmpty :: MonadIO m => IndexStream -> DBUpdateChan -> DBQueryChan -> m ()
waitForAllEmpty idxStream dbUpdateChan dbQueryChan = liftIO $ atomically $ do
  -- Check index stream.
  idxPending <- readTMVar (isPending idxStream)
  check (Map.null idxPending)
  (idxCurrent, _) <- readTMVar (isCurrent idxStream)
  check (Set.null idxCurrent)

  -- Check the channels.
  isUpdateChanEmpty <- isEmptyLenChan' dbUpdateChan
  isQueryChanEmpty <- isEmptyLenChan' dbQueryChan
  check (isUpdateChanEmpty && isQueryChanEmpty)

waitForAnyNonempty :: MonadIO m => IndexStream -> DBUpdateChan -> DBQueryChan -> m ()
waitForAnyNonempty idxStream dbUpdateChan dbQueryChan = liftIO $ atomically $ do
  idxPending <- readTMVar (isPending idxStream)
  when (Map.null idxPending) $ do
    (idxCurrent, _) <- readTMVar (isCurrent idxStream)
    when (Set.null idxCurrent) $ do
      isUpdateChanEmpty <- isEmptyLenChan' dbUpdateChan
      isQueryChanEmpty <- isEmptyLenChan' dbQueryChan
      check $ not (isUpdateChanEmpty && isQueryChanEmpty)
