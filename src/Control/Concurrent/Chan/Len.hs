{-# LANGUAGE RecordWildCards #-}

module Control.Concurrent.Chan.Len
( LenChan
, newLenChan
, writeLenChan
, readLenChan
, readLenChanPreferFirst
, isEmptyLenChan
, isEmptyLenChan'
, callLenChan
, Response
, newResponse
, sendResponse
) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.IO.Class

type LenChan a = TBQueue a

queueLimit :: Int
queueLimit = 100000

newLenChan :: MonadIO m => m (LenChan a)
newLenChan = liftIO $ atomically $ newTBQueue queueLimit

writeLenChan :: MonadIO m => LenChan a -> a -> m ()
writeLenChan c v = liftIO $ atomically $ writeTBQueue c $! v

readLenChan :: MonadIO m => LenChan a -> m a
readLenChan c = liftIO $ atomically $ readTBQueue c

readLenChanPreferFirst :: MonadIO m => LenChan a -> LenChan a -> m a
readLenChanPreferFirst c1 c2 = liftIO $ atomically $
    readTBQueue c1 `orElse` readTBQueue c2

isEmptyLenChan :: MonadIO m => LenChan a -> m Bool
isEmptyLenChan = liftIO . atomically . isEmptyTBQueue

isEmptyLenChan' :: LenChan a -> STM Bool
isEmptyLenChan' = isEmptyTBQueue

callLenChan :: MonadIO m => LenChan a -> (Response b -> a) -> m b
callLenChan c cmd = do
  mResult <- liftIO newEmptyMVar
  writeLenChan c $ cmd (Response mResult)
  liftIO $ takeMVar mResult

newtype Response a = Response (MVar a)
instance Show (Response a) where
  show _ = "(Response)"

newResponse :: MonadIO m => m (Response a)
newResponse = do
  resp <- liftIO newEmptyMVar
  return $! Response resp

sendResponse :: MonadIO m => Response a -> a -> m ()
sendResponse (Response r) v = liftIO $ putMVar r $! v
