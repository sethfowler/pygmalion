module Control.Concurrent.Chan.Counting
( CountingChan
, newCountingChan
, writeCountingChan
, readCountingChan
, readCountingChanPreferFirst
, getChanCount
) where

import Control.Concurrent.STM

data CountingChan a = CountingChan
    { queue   :: TQueue a
    , counter :: TVar Int
    }

newCountingChan :: IO (CountingChan a)
newCountingChan = do
  ch <- atomically $ newTQueue
  count <- atomically $ newTVar 0
  return $! CountingChan ch count

writeCountingChan :: CountingChan a -> a -> IO ()
writeCountingChan c v = atomically $ do
  writeTQueue (queue c) $! v
  curCount <- readTVar (counter c)
  writeTVar (counter c) $! (curCount  +1)

readCountingChan :: CountingChan a -> IO a
readCountingChan c = atomically $ do
  v <- readTQueue (queue c)
  curCount <- readTVar (counter c)
  writeTVar (counter c) $! (curCount - 1)
  return $! v

readCountingChanPreferFirst :: CountingChan a -> CountingChan a -> IO (Bool, Int, a)
readCountingChanPreferFirst c1 c2 = atomically $ do
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

getChanCount :: CountingChan a -> IO Int
getChanCount c = atomically $ readTVar (counter c)
