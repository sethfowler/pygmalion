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

data LenChan a = LenChan { queue :: TBQueue a}

queueLimit :: Int
queueLimit = 100000

newLenChan :: MonadIO m => m (LenChan a)
newLenChan = liftIO $ atomically $ do
  ch    <- newTBQueue queueLimit
  return $! LenChan ch

writeLenChan :: MonadIO m => LenChan a -> a -> m ()
writeLenChan c v = liftIO $ atomically $ writeTBQueue (queue c) $! v

readLenChan :: MonadIO m => LenChan a -> m a
readLenChan c = liftIO $ atomically $ readTBQueue (queue c)

readLenChanPreferFirst :: MonadIO m => LenChan a -> LenChan a -> m a
readLenChanPreferFirst c1 c2 = liftIO $ atomically $
    (rdQueue c1) `orElse` (rdQueue c2)
  where
   rdQueue LenChan {..} = readTBQueue queue

isEmptyLenChan :: MonadIO m => LenChan a -> m Bool
isEmptyLenChan LenChan {..} = liftIO $ atomically $ isEmptyTBQueue queue

isEmptyLenChan' :: LenChan a -> STM Bool
isEmptyLenChan' LenChan {..} = isEmptyTBQueue queue

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
