{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Database
( ensureDB
, withDB
, updateSourceFile
, getAllSourceFiles
, getSourceFile
, getSimilarSourceFile
, DBHandle
) where

import Control.Exception(bracket)
import Control.Exception.Labeled
import Data.Int
import Database.SQLite.Simple
import System.FilePath.Posix

import Pygmalion.Core

-- General database manipulation functions. These are thin wrappers around the
-- underlying database implementation that provide some additional guarantees.
type DBHandle = Connection

ensureDB :: FilePath -> IO ()
ensureDB db = withDB db (const . return $ ())

withDB :: FilePath -> (DBHandle -> IO a) -> IO a
withDB db f = bracket (openDB db) closeDB f

openDB :: FilePath -> IO DBHandle
openDB db = labeledCatch "openDB" $ do
  conn <- open db
  ensureSchema conn
  return conn

closeDB :: DBHandle -> IO ()
closeDB = close

-- Schema and operations for the Metadata table.
dbToolName :: String
dbToolName = "pygmalion"

dbMajorVersion, dbMinorVersion :: Int64
dbMajorVersion = 0
dbMinorVersion = 2

defineMetadataTable :: DBHandle -> IO ()
defineMetadataTable h = execute_ h 
  "create table if not exists Metadata(\
  \Tool varchar(2048) primary key not null, \
  \MajorVersion integer zerofill unsigned not null, \
  \MinorVersion integer zerofill unsigned not null)"

getDBVersion :: DBHandle -> IO (Maybe (Int64, Int64))
getDBVersion h = do
    row <- query h sql params
    return $ case row of
              [version] -> Just version
              _         -> Nothing
  where sql = "select MajorVersion, MinorVersion from Metadata \
              \where Tool = ?"
        params = Only dbToolName

setDBVersion :: DBHandle -> IO ()
setDBVersion h = execute h sql params
  where sql = "insert into Metadata (Tool, MajorVersion, MinorVersion) \
              \values (?, ?, ?)"
        params = (dbToolName, dbMajorVersion, dbMinorVersion)

-- Schema and operations for the SourceFiles table.
defineSourceFilesTable :: DBHandle -> IO ()
defineSourceFilesTable h = execute_ h sql
  where sql =  "create table if not exists SourceFiles(\
               \File varchar(2048) not null, \
               \Directory varchar(2048) not null, \
               \WorkingDirectory varchar(2048) not null, \
               \Command varchar(2048) not null, \
               \LastBuilt integer zerofill unsigned not null, \
               \primary key(File, Directory))"

updateSourceFile :: DBHandle -> CommandInfo -> IO ()
updateSourceFile h ci = execute h sql ci
  where sql =  "replace into SourceFiles \
               \(File, Directory, WorkingDirectory, Command, LastBuilt) \
               \values (?, ?, ?, ?, ?)"

getAllSourceFiles :: DBHandle -> IO [CommandInfo]
getAllSourceFiles h = query_ h sql
  where sql = "select File, Directory, WorkingDirectory, Command, LastBuilt \
              \from SourceFiles"

getSourceFile :: DBHandle -> FilePath -> IO (Maybe CommandInfo)
getSourceFile h f = do
    row <- query h sql (takeFileName f, takeDirectory f)
    return $ case row of
              (ci : _) -> Just ci
              _        -> Nothing
  where sql = "select File, Directory, WorkingDirectory, Command, LastBuilt \
              \from SourceFiles where File = ? and Directory = ? limit 1"

-- Eventually this should be more statistical, but right now it will just
-- return an arbitrary file from the same directory.
getSimilarSourceFile :: DBHandle -> FilePath -> IO (Maybe CommandInfo)
getSimilarSourceFile h f = do
    row <- query h sql (Only $ takeDirectory f)
    return $ case row of
              (ci : _) -> Just ci
              _        -> Nothing
  where sql = "select File, Directory, WorkingDirectory, Command, LastBuilt \
              \from SourceFiles where Directory = ? limit 1"

-- Checks that the database has the correct schema and sets it up if needed.
ensureSchema :: DBHandle -> IO ()
ensureSchema h = defineMetadataTable h
              >> defineSourceFilesTable h
              >> ensureVersion h

ensureVersion :: DBHandle -> IO ()
ensureVersion h = getDBVersion h >>= checkVersion
  where
    checkVersion (Just (major, minor))
                 | (major, minor) == (dbMajorVersion, dbMinorVersion) = return ()
                 | otherwise = throwDBVersionError major minor
    checkVersion _ = setDBVersion h

throwDBVersionError :: Int64 -> Int64 -> IO ()
throwDBVersionError major minor  =  error $ "Database version "
                                 ++ (show major) ++ "." ++ (show minor)
                                 ++ " is different than required version "
                                 ++ (show dbMajorVersion) ++ "."
                                 ++ (show dbMinorVersion)
