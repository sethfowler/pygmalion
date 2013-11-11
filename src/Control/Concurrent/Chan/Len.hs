{-# LANGUAGE RecordWildCards #-}

module Control.Concurrent.Chan.Len
( LenChan
, newLenChan
, writeLenChan
, readLenChan
, readLenChanPreferFirst
, lenChanCounter
, callLenChan
, getLenChanCount
, Response
, newResponse
, sendResponse
) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.IO.Class

data LenChan a = LenChan
    { queue   :: TBQueue a
    , counter :: TVar Int
    }

queueLimit :: Int
queueLimit = 100000

newLenChan :: MonadIO m => m (LenChan a)
newLenChan = liftIO $ atomically $ do
  ch    <- newTBQueue queueLimit
  count <- newTVar 0
  return $! LenChan ch count

writeLenChan :: MonadIO m => LenChan a -> a -> m ()
writeLenChan c v = liftIO $ atomically $ do
  writeTBQueue (queue c) $! v
  curCount <- readTVar (counter c)
  writeTVar (counter c) $! (curCount + 1)

readLenChan :: MonadIO m => LenChan a -> m (Int, a)
readLenChan c = liftIO $ atomically $ do
  v <- readTBQueue (queue c)
  curCount <- readTVar (counter c)
  let newCount = curCount - 1
  writeTVar (counter c) $! newCount
  return (newCount, v)

readLenChanPreferFirst :: MonadIO m => LenChan a -> LenChan a -> m a
readLenChanPreferFirst c1 c2 = liftIO $ atomically $
    (rdQueue c1) `orElse` (rdQueue c2)
  where
   rdQueue LenChan {..} = do val <- readTBQueue queue
                             curCount <- readTVar counter
                             writeTVar counter $ curCount - 1
                             return val

lenChanCounter :: LenChan a -> TVar Int
lenChanCounter = counter

callLenChan :: MonadIO m => LenChan a -> (Response b -> a) -> m b
callLenChan c cmd = do
  mResult <- liftIO newEmptyMVar
  writeLenChan c $ cmd (Response mResult)
  liftIO $ takeMVar mResult

getLenChanCount :: MonadIO m => LenChan a -> m Int
getLenChanCount c = liftIO $ atomically $ readTVar (counter c)

newtype Response a = Response (MVar a)
instance Show (Response a) where
  show _ = "(Response)"

newResponse :: MonadIO m => m (Response a)
newResponse = do
  resp <- liftIO newEmptyMVar
  return $! Response resp

sendResponse :: MonadIO m => Response a -> a -> m ()
sendResponse (Response r) v = liftIO $ putMVar r $! v
