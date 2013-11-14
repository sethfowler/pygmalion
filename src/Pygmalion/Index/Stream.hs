module Pygmalion.Index.Stream
( IndexStream (..)
, IndexRequestOrShutdown (..)
, mkIndexStream
, shutdownIndexStream
, addPendingIndex
, getNextFileToIndex
, finishIndexingFile
, getLastIndexedCache
, updateLastIndexedCache
, clearLastIndexedCache
) where

import Control.Monad
import Control.Concurrent.STM
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set

import Pygmalion.Core
import Pygmalion.Index.Request
import Pygmalion.Hash

data IndexStream = IndexStream
  { isCurrent          :: TMVar Set.IntSet
  , isPending          :: TMVar (Map.IntMap IndexRequest)
  , isLastIndexedCache :: TMVar (Map.IntMap CommandInfo)
  , isShouldShutdown   :: TVar Bool
  }

mkIndexStream :: IO IndexStream
mkIndexStream = do
  emptySet  <- newTMVarIO Set.empty
  emptyMap  <- newTMVarIO Map.empty
  emptyMap' <- newTMVarIO Map.empty
  shouldShutdown <- newTVarIO False
  return $! IndexStream emptySet emptyMap emptyMap' shouldShutdown

shutdownIndexStream :: IndexStream -> STM ()
shutdownIndexStream is = writeTVar (isShouldShutdown is) True

addPendingIndex :: IndexStream -> IndexRequest -> STM ()
addPendingIndex is req = do
    curPending <- takeTMVar (isPending is)
    let newPending = Map.insertWith combineReqs sfHash req curPending
    putTMVar (isPending is) newPending
  where
    sfHash = hashInt (reqSF req)

data IndexRequestOrShutdown = Index IndexRequest
                            | Shutdown
                              deriving (Show)

getNextFileToIndex :: IndexStream -> STM IndexRequestOrShutdown
getNextFileToIndex is = do
    shouldShutdown <- readTVar (isShouldShutdown is)
    if shouldShutdown then return Shutdown
                      else getNext
  where
    getNext = do
      curPending <- takeTMVar (isPending is)

      -- Retry if there's nothing available. This is the magic that lets
      -- us block until there's something to index.
      check (not $ Map.null curPending)

      -- There's something available, so grab it.
      let (sfHash, req) = Map.findMin curPending

      -- Make sure someone else isn't already working on it.
      curCurrent <- takeTMVar (isCurrent is)
      check (not $ sfHash `Set.member` curCurrent)

      -- OK, we're good to go.
      let newCurrent = sfHash `Set.insert` curCurrent
      putTMVar (isCurrent is) newCurrent
      let newPending = Map.deleteMin curPending
      putTMVar (isPending is) newPending
      
      return $ Index req

finishIndexingFile :: IndexStream -> IndexRequest -> STM ()
finishIndexingFile is req = do
    curCurrent <- takeTMVar (isCurrent is)
    let newCurrent = sfHash `Set.delete` curCurrent
    putTMVar (isCurrent is) newCurrent
  where
    sfHash = hashInt (reqSF req)

getLastIndexedCache :: IndexStream -> IndexRequest -> STM (Maybe CommandInfo)
getLastIndexedCache is req = do
    curCache <- readTMVar (isLastIndexedCache is)
    return $ sfHash `Map.lookup` curCache
  where
    sfHash = hashInt (reqSF req)
  
updateLastIndexedCache :: IndexStream -> CommandInfo -> STM ()
updateLastIndexedCache is ci = do
    curCache <- takeTMVar (isLastIndexedCache is)
    let newCache = Map.insert sfHash ci curCache
    putTMVar (isLastIndexedCache is) newCache
  where
    sfHash = hashInt (ciSourceFile ci)

clearLastIndexedCache :: IndexStream -> STM ()
clearLastIndexedCache is = void $ swapTMVar (isLastIndexedCache is) Map.empty
