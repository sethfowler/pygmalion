module Control.Concurrent.Chan.Len
( LenChan
, newLenChan
, writeLenChan
, readLenChan
, readLenChanPreferFirst
, callLenChan
, getLenChanCount
) where

import Control.Concurrent.MVar
import Control.Concurrent.STM

data LenChan a = LenChan
    { queue   :: TQueue a
    , counter :: TVar Int
    }

newLenChan :: IO (LenChan a)
newLenChan = do
  ch <- atomically $ newTQueue
  count <- atomically $ newTVar 0
  return $! LenChan ch count

writeLenChan :: LenChan a -> a -> IO ()
writeLenChan c v = atomically $ do
  writeTQueue (queue c) $! v
  curCount <- readTVar (counter c)
  writeTVar (counter c) $! (curCount + 1)

readLenChan :: LenChan a -> IO (Int, a)
readLenChan c = atomically $ do
  v <- readTQueue (queue c)
  curCount <- readTVar (counter c)
  let newCount = curCount - 1
  writeTVar (counter c) $! newCount
  return $! (newCount, v)

readLenChanPreferFirst :: LenChan a -> LenChan a -> IO (Bool, Int, a)
readLenChanPreferFirst c1 c2 = atomically $ do
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

callLenChan :: LenChan a -> (MVar b -> a) -> IO b
callLenChan c cmd = do
  mResult <- newEmptyMVar
  writeLenChan c (cmd mResult)
  takeMVar mResult

getLenChanCount :: LenChan a -> IO Int
getLenChanCount c = atomically $ readTVar (counter c)
