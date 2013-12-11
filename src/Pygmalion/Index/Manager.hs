{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards, TupleSections #-}

module Pygmalion.Index.Manager
( runIndexManager
, updateInclusions
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Hashable
import qualified Data.IntSet as Set
import Data.List (foldl')
import Data.Maybe (catMaybes)
import System.Exit
import System.Process

import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Request
import Pygmalion.File
import Pygmalion.Index.Request
import Pygmalion.Index.Stream
import Pygmalion.Log
import Pygmalion.Metadata

runIndexManager :: Config -> DBUpdateChan -> IndexStream -> IO ()
runIndexManager cf dbUpdateChan is = go
  where
    ctx = IndexContext cf is dbUpdateChan
    go = do req <- atomically $ getNextFileToIndex is
            case req of
              Index r  -> do m <- atomically $ getFileMetadata is (irFile r)
                             runReaderT (indexIfNeeded r m) ctx
                             go
              Shutdown -> logInfo "Shutting down indexing thread"

data IndexContext = IndexContext
  { icConfig       :: !Config
  , icIndexStream  :: !IndexStream
  , icDBUpdateChan :: !DBUpdateChan
  }
type Indexer a = ReaderT IndexContext IO a

finishIndexing :: SourceFile -> Indexer ()
finishIndexing sf = do
  ctx <- ask

  incs <- lift $ atomically $ cleanedInclusions (icIndexStream ctx) (hash sf)
  forM_ incs $ \i -> do
    writeLenChan (icDBUpdateChan ctx) [DBUpdateFile (fmFile i) (fmMTime i) (fmVersionHash i)]

  lift $ atomically $ finishIndexingFile (icIndexStream ctx) sf

indexIfNeeded :: IndexRequest -> Maybe FileMetadata -> Indexer ()

indexIfNeeded r (Just m) =
  -- This is an existing file. We need to determine whether it's dirty.
  case (fmCommandInfo m, irCommandInfo r,
        fmMTime m, irMTime r,
        fmVersionHash m, irVersionHash r) of

    -- Inclusion cases. (No build command.)
    (Nothing, Just newCI, _, _, _, _)
                             -> fileChanged newCI True (irMTime r <||> fmMTime m)
    (Nothing, Nothing, oldMTime, Just newMTime, _, _)
      | newMTime /= oldMTime -> inclusionChanged (fmFile m) newMTime
    (Nothing, Nothing, _, _, oldVH, Just newVH)
      | newVH /= oldVH       -> inclusionChanged (fmFile m) (irMTime r <||> fmMTime m)

    -- Source cases. (We have a build command)
    (Just oldCI, Just newCI, _, _, _, _)
      | oldCI /= newCI       -> fileChanged newCI True (irMTime r <||> fmMTime m)
    (Just ci, _, oldMTime, Just newMTime, _, _)
      | oldMTime /= newMTime -> fileChanged ci False newMTime
    (Just ci, _, _, _, oldVH, Just newVH)
      | oldVH /= newVH       -> fileChanged ci False (irMTime r <||> fmMTime m)

    _ -> ignoreUnchanged r

-- This is a new file. If we have a build command, this is a file added
-- to the index by pygmake or explicitly by the user. Otherwise,
-- this was from a file change notification, and without a build
-- command we can't index it. Currently we always ignore such files.
indexIfNeeded (IndexRequest _ (Just ci) (Just mt) _) Nothing = fileAdded ci mt
indexIfNeeded r Nothing                                      = ignoreUnknown r

(<||>) :: Maybe a -> a -> a
(<||>) (Just v) _ = v
(<||>) _ v        = v

inclusionChanged :: SourceFile -> Time -> Indexer ()
inclusionChanged sf mt = do
  ctx <- ask
  logInfo $ "Checking includers for file " ++ show sf
  metadata <- lift $ atomically $ updateInclusionMetadata (icIndexStream ctx) sf mt True
  finishIndexing sf
  -- Note that it's important that addPendingIncluders come *after* finishIndexing.
  -- This avoids a race where we can't get a lock on this inclusion to
  -- index it because we're still holding the lock while calling addPendingIncluders.
  addPendingIncluders metadata

fileChanged :: CommandInfo -> Bool -> Time -> Indexer ()
fileChanged ci ciChanged mtime = do
  let sf = ciSourceFile ci
  logInfo $ if ciChanged then "Indexing file with changed command " ++ show sf
                         else "Indexing changed file " ++ show sf
  resetFile sf
  success <- index ci
  let mtime' = if success then mtime else 0
  let ci' = if ciChanged then Just ci else Nothing
  metadata <- updateFile sf mtime' ci'
  finishIndexing sf
  addPendingIncluders metadata

fileAdded :: CommandInfo -> Time -> Indexer ()
fileAdded ci mtime = do
  let sf = ciSourceFile ci
  ctx <- ask
  logInfo $ "Indexing new file " ++ show sf
  lift $ atomically $ addSourceFileMetadata (icIndexStream ctx) ci mtime
  success <- index ci
  when success $ void $ updateFile sf mtime (Just ci)
  finishIndexing sf

addPendingIncluders :: Maybe FileMetadata -> Indexer ()
addPendingIncluders (Just m) = do
  ctx <- ask
  forM_ (Set.toList $ fmIncluders m) $ \incHash -> do
    mayFM <- lift $ atomically $ addPendingIncluderIndex (icIndexStream ctx) incHash
    case mayFM of
      Just fm -> logInfo $ "Added a pending update for dep " ++ show (fmFile fm)
      Nothing -> logWarn $ "Missing includer metadata for file: " ++ show (fmFile m)
addPendingIncluders Nothing = logWarn $ "Missing metadata when adding pending includers."

updateFile :: SourceFile -> Time -> (Maybe CommandInfo) -> Indexer (Maybe FileMetadata)
updateFile sf mt mayCI = do
  ctx <- ask
  mayFM <- lift $ atomically $ updateSourceFileMetadata (icIndexStream ctx) sf mt mayCI
  case mayFM of
    Just fm -> do let upds = DBUpdateFile sf (fmMTime fm) (fmVersionHash fm) :
                             maybe [] (pure . DBUpdateCommandInfo) mayCI
                  writeLenChan (icDBUpdateChan ctx) upds
                  return (Just fm)
    Nothing -> do logWarn $ "Missing metadata while updating file: " ++ show sf
                  return Nothing

resetFile :: SourceFile -> Indexer ()
resetFile sf = do
  ctx <- ask
  writeLenChan (icDBUpdateChan ctx) [DBResetMetadata sf]

index :: CommandInfo -> Indexer Bool
index ci = go 1
  where
    go :: Int -> Indexer Bool
    go retries = do
      ctx <- ask
      let sf = ciSourceFile ci
          conf = icConfig ctx
          
      (_, _, _, h) <- lift $ createProcess
                           (proc (idxCmd conf) [socketPath conf, show ci])
      code <- lift $ waitForProcess h

      case (code, retries) of
        (ExitSuccess, _) -> return True
        (_, 0)           -> do logInfo $ "Indexing process failed for " ++ show sf ++ "."
                               return False
        (_, _)           -> do logInfo "Indexing process failed; will retry..."
                               go (retries - 1)

ignoreUnchanged :: IndexRequest -> Indexer ()
ignoreUnchanged IndexRequest {..} = do
  logInfo $ "Not indexing unchanged file " ++ show irFile
  finishIndexing irFile

ignoreUnknown :: IndexRequest -> Indexer ()
ignoreUnknown IndexRequest {..} = do
  logInfo $ "Not indexing unknown file " ++ show irFile
  finishIndexing irFile

updateInclusions :: IndexStream -> DBUpdateChan -> SourceFileHash -> [Inclusion] ->
                    IO [SourceFileHash]
updateInclusions iStream dbUpdateChan sfHash is = do
    -- Grab the lock on as many files as possible. If any of the
    -- inclusions are already locked, another thread will take care of
    -- them, so we'll only worry about the remaining ones.
    rawCurrentIncs <- forM is $ \inc -> do
      let incHash = hash $ icInclusion inc
      res <- atomically $ acquireInclusion iStream sfHash incHash
      return $ (inc,) <$> res

    let currentIncs = catMaybes rawCurrentIncs
    --currentIncs <- atomically $ acquireInclusions iStream sfHash is

    -- Process the inclusions and determine which inclusions are actually dirty.
    (cleanIncs, dirtyIncs, reqs) <- foldM checkDirtiness ([], [], []) currentIncs

    -- Add requests to insert all of the inclusions.
    let reqs' = foldl' (\rs ic -> DBUpdateInclusion ic : rs) reqs is

    -- Send the requests.
    writeLenChan dbUpdateChan $ reverse reqs'
    
    -- Release the lock on the clean inclusions.
    atomically $ releaseInclusions iStream sfHash cleanIncs

    -- Return a list of dirty inclusions to index.
    return dirtyIncs

  where

    checkDirtiness (!cleanIncs, !dirtyIncs, !reqs) (!ic, !icHash) = do
      let icSF = icInclusion ic
      mayEntry <- atomically $ getFileMetadata iStream icSF
      case mayEntry of
        Just entry
          | fmDirty entry -> do logInfo $ "Will index dirty inclusion " ++ show icSF
                                return (cleanIncs, icHash : dirtyIncs,
                                        preIndexingUpdates icSF icHash Nothing ++ reqs)
          | otherwise     -> return (icHash : cleanIncs, dirtyIncs, reqs)
        Nothing -> do mayMTime <- getMTime icSF
                      case mayMTime of
                        Just mt -> do logInfo $ "Will index new inclusion "
                                             ++ show icSF
                                      atomically $ addInclusionMetadata iStream icSF mt
                                      return (cleanIncs, icHash : dirtyIncs,
                                              preIndexingUpdates icSF icHash (Just mt)
                                              ++ reqs)

                        Nothing -> return (icHash : cleanIncs, dirtyIncs, reqs)

preIndexingUpdates :: SourceFile -> SourceFileHash -> Maybe Time -> [DBUpdate]
preIndexingUpdates sf sfHash mayMT = reverse $
  -- Reset the metadata for this inclusion.
  DBResetMetadata sf :

  -- Add a special definition for the beginning of each inclusion.
  -- A corresponding ref is added for inclusion directives.
  DBUpdateDef (DefUpdate sf sfHash sfHash 1 1 SourceFile 0) :

  -- Update the mtime. We don't update the version hash until we're done.
  case mayMT of
    Just mt -> [DBUpdateFile sf mt 0]
    Nothing -> []
