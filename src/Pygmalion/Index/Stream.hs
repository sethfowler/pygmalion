{-# LANGUAGE BangPatterns #-}

module Pygmalion.Index.Stream
( IndexStream (..)
, IndexRequestOrShutdown (..)
, Pair (..)
, mkIndexStream
, shutdownIndexStream
, getFileMetadata
, getCommandInfoMetadata
, addSourceFileMetadata
, updateSourceFileMetadata
, addInclusionMetadata
, updateInclusionMetadata
, addPendingIndex
, addPendingIncluderIndex
, getNextFileToIndex
, finishIndexingFile
--, acquireInclusions
, acquireInclusion
, releaseInclusions
, cleanedInclusions
) where

import Control.Applicative
import Control.Concurrent.STM
import Data.Maybe
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set

import Pygmalion.Core
import Pygmalion.Index.Request
import Pygmalion.Metadata

type CurrentSet = Set.IntSet
type CurrentInclusionsMap = Map.IntMap Set.IntSet
data Pair a b = Pair !a !b

data IndexStream = IndexStream
  { isCurrent           :: TMVar (Pair CurrentSet CurrentInclusionsMap)
  , isPending           :: TMVar (Map.IntMap IndexRequest)
  , isMetadata          :: TMVar FileMetadataMap
  , isShouldShutdown    :: TVar Bool
  }

mkIndexStream :: FileMetadataMap -> IO IndexStream
mkIndexStream m = IndexStream <$> newTMVarIO (Pair Set.empty Map.empty)
                              <*> newTMVarIO Map.empty
                              <*> newTMVarIO m
                              <*> newTVarIO False

shutdownIndexStream :: IndexStream -> STM ()
shutdownIndexStream is = writeTVar (isShouldShutdown is) True

getFileMetadata :: IndexStream -> SourceFile -> STM (Maybe FileMetadata)
getFileMetadata is sf = do
    curMetadata <- readTMVar (isMetadata is)
    return $ Map.lookup sfHash curMetadata
  where
    sfHash = stableHash sf
    
getCommandInfoMetadata :: IndexStream -> SourceFile -> STM (Maybe CommandInfo)
getCommandInfoMetadata is sf = do
    curMetadata <- readTMVar (isMetadata is)
    return $ getCommandInfoForFile curMetadata sfHash
  where
    sfHash = stableHash sf

addSourceFileMetadata :: IndexStream -> CommandInfo -> Time -> STM ()
addSourceFileMetadata is ci mt = do
    curMetadata <- takeTMVar (isMetadata is)
    putTMVar (isMetadata is) (Map.insert sfHash entry curMetadata)
  where
    sfHash = stableHash (ciSourceFile ci)
    entry = newSourceEntry ci mt

updateSourceFileMetadata :: IndexStream -> SourceFile -> Time -> Maybe CommandInfo
                         -> STM (Maybe FileMetadata)
updateSourceFileMetadata is sf !mt mayCI = do
    curMetadata <- takeTMVar (isMetadata is)
    let newMetadata = Map.adjust (updater curMetadata) sfHash curMetadata
    putTMVar (isMetadata is) newMetadata
    return $ sfHash `Map.lookup` newMetadata
  where
    sfHash = stableHash sf
    updater files entry = let !vh = rehashVersion files mt entry
                          in case mayCI of
                                Just !ci -> entry { fmMTime = mt
                                                  , fmCommandInfo = Just ci
                                                  , fmVersionHash = vh
                                                  }
                                Nothing -> entry { fmMTime = mt , fmVersionHash = vh } 

addInclusionMetadata :: IndexStream -> SourceFile -> Time -> STM ()
addInclusionMetadata is sf mt = do
    curMetadata <- takeTMVar (isMetadata is)
    putTMVar (isMetadata is) (Map.insert sfHash entry curMetadata)
  where
    sfHash = stableHash sf
    entry = newInclusionEntry sf mt

updateInclusionMetadata :: IndexStream -> SourceFile -> Time -> Bool -> STM (Maybe FileMetadata)
updateInclusionMetadata is sf !mt !isDirty = do
    curMetadata <- takeTMVar (isMetadata is)
    let newMetadata = Map.adjust (updater curMetadata) sfHash curMetadata
    putTMVar (isMetadata is) newMetadata
    return $ sfHash `Map.lookup` newMetadata
  where
    sfHash = stableHash sf
    updater files entry = let !vh = rehashVersion files mt entry
                          in entry { fmMTime = mt
                                   , fmVersionHash = vh
                                   , fmDirty = isDirty
                                   }

addPendingIndex :: IndexStream -> IndexRequest -> STM ()
addPendingIndex is req = do
    curPending <- takeTMVar (isPending is)
    let newPending = Map.insertWith combineReqs sfHash req curPending
    putTMVar (isPending is) newPending
  where
    sfHash = stableHash (irFile req)

addPendingIncluderIndex :: IndexStream -> SourceFileHash -> STM (Maybe FileMetadata)
addPendingIncluderIndex is sfHash = do
    curMetadata <- readTMVar (isMetadata is)
    case sfHash `Map.lookup` curMetadata of
      Just entry -> do let !vh = rehashVersion curMetadata (fmMTime entry) entry
                       addPendingIndex is $ indexRequestForDepChange (fmFile entry) vh
                       return (Just entry)
      Nothing    -> return Nothing

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
      Pair curCurrent curIncs <- takeTMVar (isCurrent is)
      check (not $ sfHash `Set.member` curCurrent)

      -- Update the current and pending sets. We now have ownership.
      let newCurrent = sfHash `Set.insert` curCurrent
      putTMVar (isCurrent is) $ Pair newCurrent curIncs
      let newPending = Map.deleteMin curPending
      putTMVar (isPending is) newPending
      
      return $ Index req

{-
      -- Grab metadata for this file.
      metadata <- readTMVar (isMetadata is)
      let fileMetadata = Map.lookup sfHash metadata

-}

{-
acquireInclusions :: IndexStream -> SourceFileHash -> [Inclusion]
                  -> STM [(Inclusion, SourceFileHash)]
acquireInclusions is sfHash incs = do
  -- Filter out inclusions which someone else is already working on.
  curCurrent <- takeTMVar (isCurrent is)
  let hashedIncs = map (\i -> (i, hash $ icInclusion i)) incs
  let dirtyIncs = filter (not . (`Set.member` curCurrent) . snd) hashedIncs

  -- Take ownership of the new inclusions.
  let dirtyIncHashes = Set.fromList $ map snd dirtyIncs
  check (Set.null $ dirtyIncHashes `Set.intersection` curCurrent)
  let newCurrent = curCurrent `Set.union` dirtyIncHashes
  putTMVar (isCurrent is) newCurrent

  -- Ensure that the inclusions get released when finishIndexingFile gets called.
  curCurrentInclusions <- takeTMVar (isCurrentInclusions is)
  let newCurrentInclusions = Map.insertWith Set.union sfHash dirtyIncHashes curCurrentInclusions
  putTMVar (isCurrentInclusions is) newCurrentInclusions

  return dirtyIncs
-}

acquireInclusion :: IndexStream -> SourceFileHash -> SourceFileHash
                 -> STM (Maybe SourceFileHash)
acquireInclusion is sfHash incHash = do
  -- Filter out inclusions which someone else is already working on.
  Pair curCurrent curIncs <- takeTMVar (isCurrent is)
  if not (incHash `Set.member` curCurrent)
     then do let newIncs = Map.insertWith Set.union sfHash
                                          (Set.singleton incHash) curIncs
                 newCurrent = incHash `Set.insert` curCurrent
             putTMVar (isCurrent is) $ Pair newCurrent newIncs
             return $ Just incHash
     else do putTMVar (isCurrent is) $ Pair curCurrent curIncs
             return Nothing

releaseInclusions :: IndexStream -> SourceFileHash -> [SourceFileHash] -> STM ()
releaseInclusions is sfHash incs = do
  Pair curCurrent curIncs <- takeTMVar (isCurrent is)
  let incSet = Map.findWithDefault Set.empty sfHash curIncs

  -- Remove the passed-in inclusions from both the set of current
  -- files and the set of inclusions associated with this file.
  let incsToRemove = Set.fromList incs
  let newCurrent = curCurrent `Set.difference` incsToRemove
  let newIncSet = incSet `Set.difference` incsToRemove

  -- If we removed _all_ the inclusions, we can go ahead and remove
  -- this file's entry from the inclusion map. Otherwise, we replace
  -- the old inclusion map entry with the new one calculated above.
  let newIncs = if Set.null newIncSet
                  then sfHash `Map.delete` curIncs
                  else Map.insert sfHash newIncSet curIncs

  putTMVar (isCurrent is) $ Pair newCurrent newIncs

finishIndexingFile :: IndexStream -> SourceFile -> STM ()
finishIndexingFile is sf = do
    -- Release ownership of this file and any associated inclusions.
    Pair curCurrent curIncs <- takeTMVar (isCurrent is)
    let incSet = Map.findWithDefault Set.empty sfHash curIncs
    let newCurrent = sfHash `Set.delete` (curCurrent `Set.difference` incSet)
    let newIncs = sfHash `Map.delete` curIncs
    putTMVar (isCurrent is) $ Pair newCurrent newIncs
  where
    sfHash = stableHash sf

cleanedInclusions :: IndexStream -> SourceFileHash -> STM [FileMetadata]
cleanedInclusions is sfHash = do
    Pair _ curIncs <- readTMVar (isCurrent is)
    let incSet = Map.findWithDefault Set.empty sfHash curIncs

    -- Update metadata of current inclusions.
    if not (Set.null incSet)
       then do curMetadata <- takeTMVar (isMetadata is)
               let newMetadata = Set.fold (Map.adjust updater) curMetadata incSet
               putTMVar (isMetadata is) newMetadata
               return $ mapMaybe (`Map.lookup` newMetadata) (Set.toList incSet)
       else return []
  where
    updater entry = entry { fmDirty = False }
