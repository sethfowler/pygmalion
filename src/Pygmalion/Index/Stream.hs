module Pygmalion.Index.Stream
( IndexStream (..)
, IndexRequestOrShutdown (..)
, Dirtiness (..)
, mkIndexStream
, shutdownIndexStream
, addPendingIndex
, addPendingDepIndex
, getNextFileToIndex
, finishIndexingFile
, dirtinessCacheLookup
, updateDirtinessCache
, commandInfoCacheLookup
, updateCommandInfoCache
, clearCaches
, acquireInclusions
, releaseInclusions
) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.STM
import Data.Hashable
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set

import Pygmalion.Core
import Pygmalion.Index.Request

data IndexStream = IndexStream
  { isCurrent           :: TMVar Set.IntSet
  , isPending           :: TMVar (Map.IntMap IndexRequest)
  , isCurrentInclusions :: TMVar (Map.IntMap Set.IntSet)
  , isDirtinessCache    :: TMVar (Map.IntMap Dirtiness)
  , isCommandInfoCache  :: TMVar (Map.IntMap CommandInfo)
  , isShouldShutdown    :: TVar Bool
  }

-- The third possibility, that the file itself is dirty, is
-- represented by there being no entry for that file in the dirtiness cache.
data Dirtiness = HasChangedDep
               | HasNotChanged
                 deriving (Eq, Show)

mkIndexStream :: IO IndexStream
mkIndexStream = IndexStream <$> newTMVarIO Set.empty
                            <*> newTMVarIO Map.empty
                            <*> newTMVarIO Map.empty
                            <*> newTMVarIO Map.empty
                            <*> newTMVarIO Map.empty
                            <*> newTVarIO False

shutdownIndexStream :: IndexStream -> STM ()
shutdownIndexStream is = writeTVar (isShouldShutdown is) True

addPendingIndex :: IndexStream -> IndexRequest -> STM ()
addPendingIndex is req = do
    case req of
      IndexUpdate _ -> do curDirty <- takeTMVar (isDirtinessCache is)
                          let newDirty = Map.delete sfHash curDirty
                          putTMVar (isDirtinessCache is) newDirty
      _             -> return ()

    curPending <- takeTMVar (isPending is)
    let newPending = Map.insertWith combineReqs sfHash req curPending
    putTMVar (isPending is) newPending
  where
    sfHash = hash (reqSF req)

addPendingDepIndex :: IndexStream -> IndexRequest -> STM ()
addPendingDepIndex is req = do
    case req of
      IndexUpdate _ -> do curDirty <- takeTMVar (isDirtinessCache is)
                          let newDirty = Map.insert sfHash HasChangedDep curDirty
                          putTMVar (isDirtinessCache is) newDirty
      _             -> return ()

    curPending <- takeTMVar (isPending is)
    let newPending = Map.insertWith combineReqs sfHash req curPending
    putTMVar (isPending is) newPending
  where
    sfHash = hash (reqSF req)

data IndexRequestOrShutdown = Index !IndexRequest
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

acquireInclusions :: IndexStream -> SourceFileHash -> [Inclusion]
                  -> STM [(Inclusion, SourceFileHash)]
acquireInclusions is sfHash incs = do
  -- Filter out inclusions which someone else is already working on.
  curCurrent <- takeTMVar (isCurrent is)
  let hashedIncs = map (\i -> (i, hash $ icInclusion i)) incs
  let dirtyIncs = filter (not . (`Set.member` curCurrent) . snd) hashedIncs

  -- Take ownership of the new inclusions.
  let dirtyIncHashes = Set.fromList $ map snd dirtyIncs
  let newCurrent = curCurrent `Set.union` dirtyIncHashes
  putTMVar (isCurrent is) newCurrent

  -- Ensure that the inclusions get released when finishIndexingFile gets called.
  curCurrentInclusions <- takeTMVar (isCurrentInclusions is)
  let newCurrentInclusions = Map.insertWith Set.union sfHash dirtyIncHashes curCurrentInclusions
  putTMVar (isCurrentInclusions is) newCurrentInclusions

  return dirtyIncs

releaseInclusions :: IndexStream -> SourceFileHash -> [SourceFileHash] -> STM ()
releaseInclusions is sfHash incs = do
  curCurrent <- takeTMVar (isCurrent is)
  curCurrentInclusions <- takeTMVar (isCurrentInclusions is)
  let incSet = Map.findWithDefault Set.empty sfHash curCurrentInclusions
  let newCurrent = curCurrent `Set.difference` incSet
  let newIncSet = incSet `Set.difference` Set.fromList incs
  let newCurrentInclusions = if Set.null newIncSet
                             then sfHash `Map.delete` curCurrentInclusions
                             else Map.insert sfHash newIncSet curCurrentInclusions
  putTMVar (isCurrentInclusions is) newCurrentInclusions
  putTMVar (isCurrent is) newCurrent

finishIndexingFile :: IndexStream -> SourceFile -> STM ()
finishIndexingFile is sf = do
    -- Release ownership of this file and any associated inclusions.
    curCurrent <- takeTMVar (isCurrent is)
    curCurrentInclusions <- takeTMVar (isCurrentInclusions is)
    let incSet = Map.findWithDefault Set.empty sfHash curCurrentInclusions
    let newCurrent = sfHash `Set.delete` (curCurrent `Set.difference` incSet)
    let newCurrentInclusions = sfHash `Map.delete` curCurrentInclusions
    putTMVar (isCurrentInclusions is) newCurrentInclusions
    putTMVar (isCurrent is) newCurrent
  where
    sfHash = hash sf

dirtinessCacheLookup :: IndexStream -> SourceFile -> STM (Maybe Dirtiness)
dirtinessCacheLookup is sf = do
    curCache <- readTMVar (isDirtinessCache is)
    return $ sfHash `Map.lookup` curCache
  where
    sfHash = hash sf
  
updateDirtinessCache :: IndexStream -> SourceFile -> Dirtiness -> STM ()
updateDirtinessCache is sf dirtiness = do
    curCache <- takeTMVar (isDirtinessCache is)
    let newCache = Map.insert sfHash dirtiness curCache
    putTMVar (isDirtinessCache is) newCache
  where
    sfHash = hash sf

commandInfoCacheLookup :: IndexStream -> SourceFile -> STM (Maybe CommandInfo)
commandInfoCacheLookup is sf = do
    curCache <- readTMVar (isCommandInfoCache is)
    return $ sfHash `Map.lookup` curCache
  where
    sfHash = hash sf

updateCommandInfoCache :: IndexStream -> CommandInfo -> STM ()
updateCommandInfoCache is ci = do
    curCache <- takeTMVar (isCommandInfoCache is)
    let newCache = Map.insert sfHash ci curCache
    putTMVar (isCommandInfoCache is) newCache
  where
    sfHash = hash (ciSourceFile ci)

clearCaches :: IndexStream -> STM ()
clearCaches is = do
  void $ swapTMVar (isDirtinessCache is) Map.empty
  void $ swapTMVar (isCommandInfoCache is) Map.empty
