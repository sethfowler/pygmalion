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
, acquireInclusions
, releaseInclusions
, fileDirtiness
, FileDirtiness (..)
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Concurrent.STM
import Data.Hashable
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import Data.Time.Clock.POSIX
import System.Directory

import Pygmalion.Core
import Pygmalion.Index.Extension
import Pygmalion.Index.Request
import Pygmalion.Log

data IndexStream = IndexStream
  { isCurrent           :: TMVar Set.IntSet
  , isPending           :: TMVar (Map.IntMap IndexRequest)
  , isCurrentInclusions :: TMVar (Map.IntMap Set.IntSet)
  , isLastIndexedCache  :: TMVar (Map.IntMap CommandInfo)
  , isShouldShutdown    :: TVar Bool
  }

mkIndexStream :: IO IndexStream
mkIndexStream = IndexStream <$> newTMVarIO Set.empty
                            <*> newTMVarIO Map.empty
                            <*> newTMVarIO Map.empty
                            <*> newTMVarIO Map.empty
                            <*> newTVarIO False

shutdownIndexStream :: IndexStream -> STM ()
shutdownIndexStream is = writeTVar (isShouldShutdown is) True

addPendingIndex :: IndexStream -> IndexRequest -> STM ()
addPendingIndex is req = do
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
  curCurrentInclusions <- takeTMVar (isCurrentInclusions is)
  let incSet = Map.findWithDefault Set.empty sfHash curCurrentInclusions
  let newIncSet = incSet `Set.difference` Set.fromList incs
  let newCurrentInclusions = if Set.null newIncSet
                             then sfHash `Map.delete` curCurrentInclusions
                             else Map.insert sfHash newIncSet curCurrentInclusions
  putTMVar (isCurrentInclusions is) newCurrentInclusions

finishIndexingFile :: IndexStream -> SourceFile -> STM ()
finishIndexingFile is sf = do
    -- Release ownership of this file and any associated inclusions.
    curCurrent <- takeTMVar (isCurrent is)
    curCurrentInclusions <- takeTMVar (isCurrentInclusions is)
    let incSet = Map.findWithDefault Set.empty sfHash curCurrentInclusions
    let newCurrent = sfHash `Set.delete` (curCurrent `Set.difference` incSet)
    let newCurrentInclusions = sfHash `Map.delete` curCurrentInclusions
    putTMVar (isCurrent is) newCurrent
    putTMVar (isCurrentInclusions is) newCurrentInclusions
  where
    sfHash = hash sf

getLastIndexedCache :: IndexStream -> SourceFile -> STM (Maybe CommandInfo)
getLastIndexedCache is sf = do
    curCache <- readTMVar (isLastIndexedCache is)
    return $ sfHash `Map.lookup` curCache
  where
    sfHash = hash sf
  
updateLastIndexedCache :: IndexStream -> CommandInfo -> STM ()
updateLastIndexedCache is ci = do
    curCache <- takeTMVar (isLastIndexedCache is)
    let newCache = Map.insert sfHash ci curCache
    putTMVar (isLastIndexedCache is) newCache
  where
    sfHash = hash (ciSourceFile ci)

clearLastIndexedCache :: IndexStream -> STM ()
clearLastIndexedCache is = void $ swapTMVar (isLastIndexedCache is) Map.empty

data FileDirtiness = NewFile Time CommandInfo           -- index
                   | NewInclusion Time                  -- handled by the indexing process
                   | CommandChanged Time CommandInfo    -- checkDeps, reset, index
                   | FileChanged Time CommandInfo       -- checkDeps, reset, index
                   | DependencyChanged Time CommandInfo -- reset, index
                   | InclusionChanged Time              -- handled by indexing process
                   | Unchanged Time
                   | Unreadable
                   | Unknown
                     deriving (Eq, Show)

-- Trigger indexing if this file is dirty. This first version of the
-- function makes whatever decisions we can make without hitting the
-- disk or the database.
fileDirtiness :: IndexRequest -> IO (Maybe CommandInfo) -> Maybe CommandInfo -> IO FileDirtiness
fileDirtiness req getCI mayLastCI =
  -- In general if we have a cached CI we make a decision here as long
  -- as we haven't seen a FromNotify for the file.
  case (req, mayLastCI) of
    (FromBuild ci False, Just lastCI)
      | commandInfoChanged ci lastCI -> return $ CommandChanged (ciLastMTime lastCI) lastCI
      | otherwise                    -> return $ Unchanged (ciLastMTime lastCI)
    (FromDepChange ci t False, Just lastCI)
      | ciLastIndexed lastCI < t     -> return $ DependencyChanged (ciLastMTime lastCI) ci
      | otherwise                    -> return $ Unchanged (ciLastMTime lastCI)
    _                                -> fileDirtiness' req getCI mayLastCI

-- This second version of the function makes the decisions we can make
-- where reading the mtime is required, but hitting the database isn't.
fileDirtiness' :: IndexRequest -> IO (Maybe CommandInfo) -> Maybe CommandInfo
               -> IO FileDirtiness
fileDirtiness' req getCI mayLastCI = do
  mayMTime <- getMTime $ reqSF req

  case (req, mayLastCI, mayMTime) of
    (_, _, Nothing)    -> return Unreadable
    (FromBuild ci _, Just lastCI, Just mtime)
      | commandInfoChanged ci lastCI -> return $ CommandChanged mtime ci
      | ciLastMTime lastCI /= mtime  -> return $ FileChanged mtime ci
      | otherwise                    -> return $ Unchanged mtime
    (FromNotify _, Just lastCI, Just mtime)
      | ciLastMTime lastCI /= mtime  -> return $ FileChanged mtime lastCI
      | otherwise                    -> return $ Unchanged mtime
    (FromDepChange ci t True, Just lastCI, Just mtime)
      | ciLastIndexed lastCI < t     -> return $ DependencyChanged mtime ci
      | ciLastMTime lastCI /= mtime  -> return $ DependencyChanged mtime ci
                                        -- All dependencies will be reindexed anyway.
      | otherwise                    -> return $ Unchanged mtime
    (FromInclusion _, Just lastCI, Just mtime)
      | ciLastMTime lastCI /= mtime  -> return $ InclusionChanged mtime
      | otherwise                    -> return $ Unchanged mtime
    (_, _, Just mtime)               -> fileDirtiness'' req getCI mtime

-- This final version of the function is used only if we don't get a
-- cache hit. We're forced to hit the database to make a final decision.
fileDirtiness'' :: IndexRequest -> IO (Maybe CommandInfo) -> Time -> IO FileDirtiness
fileDirtiness'' req getCI mtime = do
  mayOldCI <- getCI --callLenChan (icDBQueryChan ctx) $ DBGetCommandInfo (reqSF req)

  case (req, mayOldCI) of
    (FromBuild ci _, Just oldCI)
      | commandInfoChanged ci oldCI  -> return $ CommandChanged mtime ci
      | ciLastMTime oldCI /= mtime   -> return $ FileChanged mtime ci
      | otherwise                    -> return $ Unchanged mtime
    (FromBuild ci _, Nothing)        -> return $ NewFile mtime ci
    (FromNotify _, Just oldCI)
      | ciLastMTime oldCI /= mtime   -> return $ FileChanged mtime oldCI
      | otherwise                    -> return $ Unchanged mtime
    (FromNotify _, Nothing)          -> return Unknown
    (FromDepChange ci t _, Just oldCI)
      | ciLastIndexed oldCI < t      -> return $ DependencyChanged mtime ci
      | ciLastMTime oldCI /= mtime   -> return $ DependencyChanged mtime ci
                                        -- All dependencies will be reindexed anyway.
      | otherwise                    -> return $ Unchanged mtime
    (FromDepChange ci _ _, Nothing)  -> return $ DependencyChanged mtime ci
    (FromInclusion _, Just oldCI)
      | ciLastMTime oldCI /= mtime   -> return $ InclusionChanged mtime
      | otherwise                    -> return $ Unchanged mtime
    (FromInclusion _, Nothing)       -> return $ NewInclusion mtime

commandInfoChanged :: CommandInfo -> CommandInfo -> Bool
commandInfoChanged a b | not (hasSourceExtension $ ciSourceFile a) = False  -- Hack until we modify how inclusions work.
                       | ciWorkingPath a /= ciWorkingPath b = True
                       | ciCommand a     /= ciCommand b     = True
                       | ciArgs a        /= ciArgs b        = True
                       | ciLanguage a    /= ciLanguage b    = True
                       | otherwise                          = False

getMTime :: SourceFile -> IO (Maybe Time)
getMTime sf = do
  result <- try $ getModificationTime (unSourceFile sf)
  case result of
    Right clockTime -> return . Just . floor . utcTimeToPOSIXSeconds $ clockTime
    Left e          -> do logInfo $ "Couldn't read mtime for file "
                                 ++ unSourceFile sf ++ ": "
                                 ++ show (e :: IOException)
                          return Nothing  -- Most likely the file has been deleted.
