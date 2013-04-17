module Control.Concurrent.Chan.Counting
( CountingChan
, newCountingChan
, writeCountingChan
, readCountingChan
, getChanCount
) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar

data CountingChan a = CountingChan
    { chan :: Chan a
    , counter :: MVar Int
    }

newCountingChan :: IO (CountingChan a)
newCountingChan = do
  ch <- newChan
  count <- newMVar 0
  return $! CountingChan ch count

writeCountingChan :: CountingChan a -> a -> IO ()
writeCountingChan c v = do
  writeChan (chan c) $! v
  curCount <- takeMVar (counter c)
  let newCount = curCount + 1
  putMVar (counter c) $! newCount

readCountingChan :: CountingChan a -> IO a
readCountingChan c = do
  v <- readChan (chan c)
  curCount <- takeMVar (counter c)
  let newCount = curCount - 1
  putMVar (counter c) $! newCount
  return $! v

getChanCount :: CountingChan a -> IO Int
getChanCount c = readMVar (counter c)
