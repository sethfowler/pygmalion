module Control.Concurrent.Chan.Len
( LenChan
, newLenChan
, writeLenChan
, readLenChan
, readLenChanPreferFirst
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
    { queue   :: TQueue a
    , counter :: TVar Int
    }

newLenChan :: MonadIO m => m (LenChan a)
newLenChan = liftIO $ atomically $ do
  ch    <- newTQueue
  count <- newTVar 0
  return $! LenChan ch count

writeLenChan :: MonadIO m => LenChan a -> a -> m ()
writeLenChan c v = liftIO $ atomically $ do
  writeTQueue (queue c) $! v
  curCount <- readTVar (counter c)
  writeTVar (counter c) $! (curCount + 1)

readLenChan :: MonadIO m => LenChan a -> m (Int, a)
readLenChan c = liftIO $ atomically $ do
  v <- readTQueue (queue c)
  curCount <- readTVar (counter c)
  let newCount = curCount - 1
  writeTVar (counter c) $! newCount
  return $! (newCount, v)

readLenChanPreferFirst :: MonadIO m => LenChan a -> LenChan a -> m (Bool, Int, a)
readLenChanPreferFirst c1 c2 = liftIO $ atomically $ do
  v1 <- tryReadTQueue (queue c1)
  case v1 of
    Just value -> do curCount <- readTVar (counter c1)
                     let newCount = curCount - 1
                     writeTVar (counter c1) $! newCount
                     return $! (True, newCount, value)
    Nothing    -> do v2 <- readTQueue (queue c2)
                     curCount <- readTVar (counter c2)
                     let newCount = curCount - 1
                     writeTVar (counter c2) $! newCount
                     return $! (False, newCount, v2)

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
