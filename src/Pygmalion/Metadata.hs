{-# LANGUAGE BangPatterns #-}

module Pygmalion.Metadata
( FileMetadataMap
, FileSet
, FileMetadata (..)
, readMetadata
, checkMetadata
, rehashVersion
, hasBuildCommand
, newSourceEntry
, newInclusionEntry
, getCommandInfoForFile
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set

import Pygmalion.Core
import Pygmalion.Database.IO

type FileMetadataMap = Map.IntMap FileMetadata
type FileSet = Set.IntSet

-- The 'version hash' is the hash of this file's last mtime combined
-- with the version hashes of all of its inclusions. In this way leaf
-- inclusions have version hashes that depend only on their last
-- mtime, while other inclusions depend on their own last mtime and
-- the last mtimes of all their dependencies.
-- The version hash neatly solves the problem of triggering indexing
-- for dependencies, even in the presence of cyclic dependencies.
-- When we send a DepChanged index request, we compute the version
-- hash we _want_ the file to have. Since we only send DepChanged
-- requests one level down the tree, this is easy. We include that
-- version hash in the DepChanged index request. If the file's version
-- hash doesn't match when we handle the request, then indexing
-- happens and we trigger DepChanged requests for its includers. We
-- also update the version hash so that even if we cycle around to
-- that file again, we now have the correct version hash and nothing happens.
-- Filesystem notifications still use mtime directly as usual.

data FileMetadata = FileMetadata
  { fmFile        :: !SourceFile
  , fmMTime       :: !Time
  , fmVersionHash :: !TimeHash
  , fmIncluders   :: !FileSet
  , fmInclusions  :: !FileSet
  , fmDirty       :: !Bool
  , fmCommandInfo :: !(Maybe CommandInfo)
  } deriving (Eq, Show)

hasBuildCommand :: FileMetadata -> Bool
hasBuildCommand (FileMetadata _ _ _ _ _ _ (Just _)) = True
hasBuildCommand _                                   = False

newSourceEntry :: CommandInfo -> Time -> FileMetadata
newSourceEntry !ci !mt = FileMetadata (ciSourceFile ci) mt 0 Set.empty Set.empty False (Just ci)

newInclusionEntry :: SourceFile -> Time -> FileMetadata
newInclusionEntry !sf !mt = FileMetadata sf mt 0 Set.empty Set.empty False Nothing
    
getCommandInfoForFile :: FileMetadataMap -> SourceFileHash -> Maybe CommandInfo
getCommandInfoForFile = go Set.empty
  where
    go visited files sfHash
      | sfHash `Set.member` visited = Nothing
      | otherwise =
          case Map.lookup sfHash files of
            Just m  -> fmCommandInfo m
                   <|> getIncluderCI (sfHash `Set.insert` visited) files (fmIncluders m)
            Nothing -> Nothing

    getIncluderCI visited fs is = foldr (<|>) Nothing $ map (go visited fs) (Set.toList is)

-- Reads file metadata from the database and creates the data structure
-- we query at runtime. Note that it is not safe to run this at the
-- same time as the database manager since it accesses the database directly.
-- Since this is only executed once, at startup, this shouldn't be a problem.
readMetadata :: IO FileMetadataMap
readMetadata = withDB $ \h -> do
  files <- getAllFiles h
  foldM (addEntryFromDB h) Map.empty files
  
addEntryFromDB :: DBHandle -> FileMetadataMap -> (SourceFile, Time, TimeHash)
               -> IO FileMetadataMap
addEntryFromDB h files (sf, mtime, versionHash) = do
    let sfHash = stableHash sf

    -- Gather information about inclusions.
    includers <- getDirectIncluderHashes h sfHash
    inclusions <- getDirectInclusionHashes h sfHash

    -- Look up command info, if any.
    mayCI <- getCommandInfo h sfHash

    -- Create the complete entry.
    let includerSet  = Set.fromList includers
        inclusionSet = Set.fromList inclusions
        newEntry     = FileMetadata sf mtime versionHash
                                    includerSet inclusionSet
                                    False mayCI

    -- Merge it into the map.
    return $ Map.insert sfHash newEntry files

rehashVersion :: FileMetadataMap -> Time -> FileMetadata -> TimeHash
rehashVersion !files !mtime !entry = foldl' stableHashWithSalt (stableHash mtime) (mtimes entry)
  where
    mTimeHash = stableHash . fmMTime
    lookupFile = (`Map.lookup` files)
    mtimes = map mTimeHash . mapMaybe lookupFile . Set.toList . fmInclusions

-- Returns a list of strings describing any inconsistencies which are
-- found in the given metadata.
checkMetadata :: FileMetadataMap -> [String]
checkMetadata files = execWriter $ go (Map.elems files)
  where
    go []           = return ()
    go (entry : es) = do
      let sfHash = stableHash (fmFile entry)
          
      -- Verify includer consistency.
      forM_ (Set.elems $ fmIncluders entry) $ \i ->
        case Map.lookup i files of
          Just includer -> unless (sfHash `Set.member` fmInclusions includer) $
                             nonSymmetricIncluderErr entry includer
                              -- checkLoops [entry] (Set.singleton sfHash) includer
          Nothing       -> nonexistentIncluderErr entry i
        
      -- Verify inclusion consistency.
      forM_ (Set.elems $ fmInclusions entry) $ \i ->
        case Map.lookup i files of
          Just inclusion -> unless (sfHash `Set.member` fmIncluders inclusion) $
                             nonSymmetricInclusionErr entry inclusion
          Nothing        -> nonexistentInclusionErr entry i

      -- Verify CommandInfo consistency.
      case fmCommandInfo entry of
        Just ci -> when (ciSourceFile ci /= fmFile entry) $
                     inconsistentCommandInfoErr entry ci
        Nothing -> return ()

      go es

    -- Commented out since loops turn out to be frequent, but this
    -- information is useful for debugging.
    {-
    checkLoops !path !visited !entry =
      if stableHash (fmSourceFile entry) `Set.member` visited 
        then inclusionLoopErr (entry : path) entry
        else let newVisited = stableHash (fmSourceFile entry) `Set.insert` visited
             in forM_ (Set.elems $ fmIncluders entry) $ \i ->
                  maybe (return ()) (checkLoops (entry : path) newVisited)
                        (Map.lookup i files)
    
    inclusionLoopErr path entry =
      tell [entryFile entry ++ " includes itself via path " ++
            show (map entryFile path)]
    -}
      
    entryFile = unSourceFile . fmFile

    nonSymmetricIncluderErr entry includer =
      tell [entryFile entry ++ " has includer " ++ entryFile includer ++
            " which doesn't list it as an inclusion."]

    nonexistentIncluderErr entry includer =
      tell [entryFile entry ++ " has nonexistent includer " ++
            show includer ++ "."]

    nonSymmetricInclusionErr entry inclusion =
      tell [entryFile entry ++ " has inclusion " ++ entryFile inclusion ++
            " which doesn't list it as an includer."]

    nonexistentInclusionErr entry inclusion =
      tell [entryFile entry ++ " has nonexistent inclusion " ++
            show inclusion ++ "."]

    inconsistentCommandInfoErr entry ci =
      tell [entryFile entry ++ " has a build command for different file " ++
            (unSourceFile $ ciSourceFile ci) ++ "."]
