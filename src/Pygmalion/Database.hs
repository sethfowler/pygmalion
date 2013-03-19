{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Database
( ensureDB
, withDB
, updateSourceFile
, getAllSourceFiles
, DBHandle
) where

import Control.Exception(bracket)
import Control.Exception.Labeled
import Data.Int
import Database.SQLite.Simple

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
defineMetadataTable c = execute_ c 
  "create table if not exists Metadata(\
  \Tool varchar(2048) primary key not null, \
  \MajorVersion integer zerofill unsigned not null, \
  \MinorVersion integer zerofill unsigned not null)"

getDBVersion :: DBHandle -> IO (Maybe (Int64, Int64))
getDBVersion c = do
    row <- query c sql params
    case row of
      [version] -> return . Just $ version
      _         -> return Nothing
  where sql = "select MajorVersion, MinorVersion from Metadata \
              \where Tool = ?"
        params = Only dbToolName

setDBVersion :: DBHandle -> IO ()
setDBVersion c = execute c sql params
  where sql = "insert into Metadata (Tool, MajorVersion, MinorVersion) \
              \values (?, ?, ?)"
        params = (dbToolName, dbMajorVersion, dbMinorVersion)

-- Schema and operations for the SourceFiles table.
defineSourceFilesTable :: DBHandle -> IO ()
defineSourceFilesTable c = execute_ c sql
  where sql =  "create table if not exists SourceFiles(\
               \File varchar(2048) not null, \
               \Directory varchar(2048) not null, \
               \WorkingDirectory varchar(2048) not null, \
               \Command varchar(2048) not null, \
               \LastBuilt integer zerofill unsigned not null, \
               \primary key(File, Directory))"

updateSourceFile :: DBHandle -> CommandInfo -> IO ()
updateSourceFile c ci = execute c sql ci
  where sql =  "replace into SourceFiles \
               \(File, Directory, WorkingDirectory, Command, LastBuilt) \
               \values (?, ?, ?, ?, ?)"

getAllSourceFiles :: DBHandle -> IO [CommandInfo]
getAllSourceFiles c = query_ c sql
  where sql = "select File, Directory, WorkingDirectory, Command, LastBuilt \
              \from SourceFiles"

-- Checks that the database has the correct schema and sets it up if needed.
ensureSchema :: DBHandle -> IO ()
ensureSchema c = defineMetadataTable c
              >> defineSourceFilesTable c
              >> ensureVersion c

ensureVersion :: DBHandle -> IO ()
ensureVersion c = getDBVersion c >>= checkVersion
  where
    checkVersion (Just (major, minor))
                 | (major, minor) == (dbMajorVersion, dbMinorVersion) = return ()
                 | otherwise = throwDBVersionError major minor
    checkVersion _ = setDBVersion c

throwDBVersionError :: Int64 -> Int64 -> IO ()
throwDBVersionError major minor  =  error $ "Database version "
                                 ++ (show major) ++ "." ++ (show minor)
                                 ++ " is different than required version "
                                 ++ (show dbMajorVersion) ++ "."
                                 ++ (show dbMinorVersion)
