{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Database
( ensureDB
, withDB
, updateSourceFile
, getAllSourceFiles
, getSourceFile
, getSimilarSourceFile
, enableTracing
, DBHandle
) where

import Control.Exception(bracket)
import Data.Int
import Data.List
import Data.String
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (ToField(..))
import System.FilePath.Posix

import Control.Exception.Labeled
import Pygmalion.Core

-- General database manipulation functions. These are thin wrappers around the
-- underlying database implementation that also verify that the database is
-- configured according to the correct schema and enable foreign keys.
type DBHandle = Connection

ensureDB :: FilePath -> IO ()
ensureDB db = withDB db (const . return $ ())

withDB :: FilePath -> (DBHandle -> IO a) -> IO a
withDB db f = bracket (openDB db) closeDB f

openDB :: FilePath -> IO DBHandle
openDB db = labeledCatch "openDB" $ do
  h <- open db
  enableForeignKeyConstraints h
  ensureSchema h
  return h

closeDB :: DBHandle -> IO ()
closeDB = close

enableForeignKeyConstraints :: DBHandle -> IO ()
enableForeignKeyConstraints h = execute_ h "pragma foreign_keys = on"

enableTracing :: DBHandle -> IO ()
enableTracing h = setTrace h (Just $ putStrLn . T.unpack)

-- Schema and operations for the Metadata table.
dbToolName :: String
dbToolName = "pygmalion"

dbMajorVersion, dbMinorVersion :: Int64
dbMajorVersion = 0
dbMinorVersion = 3

defineMetadataTable :: DBHandle -> IO ()
defineMetadataTable h = execute_ h 
  "create table if not exists Metadata(               \
  \ Tool varchar(2048) primary key not null,          \
  \ MajorVersion integer zerofill unsigned not null,  \
  \ MinorVersion integer zerofill unsigned not null)"

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

-- Schema and operations for the Paths table.
definePathsTable :: DBHandle -> IO ()
definePathsTable h = execute_ h sql
  where sql =  "create table if not exists Paths(        \
               \ Id integer primary key unique not null, \
               \ Path varchar(2048) unique not null)"

-- Schema and operations for the BuildCommands table.
defineBuildCommandsTable :: DBHandle -> IO ()
defineBuildCommandsTable h = execute_ h sql
  where sql =  "create table if not exists BuildCommands(\
               \ Id integer primary key unique not null, \
               \ Command varchar(2048) unique not null)"

-- Schema and operations for the BuildArgs table.
defineBuildArgsTable :: DBHandle -> IO ()
defineBuildArgsTable h = execute_ h sql
  where sql =  "create table if not exists BuildArgs(    \
               \ Id integer primary key unique not null, \
               \ Args varchar(2048) unique not null)"

-- Schema and operations for the SourceFiles table.
defineSourceFilesTable :: DBHandle -> IO ()
defineSourceFilesTable h = execute_ h sql
  where sql =  "create table if not exists SourceFiles(                  \
               \ File varchar(2048) not null,                            \
               \ Path integer not null,                                  \
               \ WorkingDirectory integer not null,                      \
               \ BuildCommand integer not null,                          \
               \ BuildArgs integer not null,                             \
               \ LastBuilt integer zerofill unsigned not null,           \
               \ foreign key(Path) references Paths(Id),                 \
               \ foreign key(WorkingDirectory) references Paths(Id),     \
               \ foreign key(BuildCommand) references BuildCommands(Id), \
               \ foreign key(BuildArgs) references BuildArgs(Id),        \
               \ primary key(File, Path))"

updateSourceFile :: DBHandle -> CommandInfo -> IO ()
updateSourceFile h (CommandInfo f wd (Command cmd args) t) = do
    execute_ h "begin transaction"
    pathId <- getIdForRow h "Paths" "Path" (normalise . takeDirectory $ f)
    wdId <- getIdForRow h "Paths" "Path" (normalise wd)
    cmdId <- getIdForRow h "BuildCommands" "Command" cmd
    argsId <- getIdForRow h "BuildArgs" "Args" (intercalate " " args)
    execute_ h "commit"
    execute h sql (normalise . takeFileName $ f, pathId, wdId, cmdId, argsId, t)
  where
    sql =  "replace into SourceFiles                                           \
           \(File, Path, WorkingDirectory, BuildCommand, BuildArgs, LastBuilt) \
           \values (?, ?, ?, ?, ?, ?)"

getIdForRow :: ToField a => DBHandle -> String -> String -> a -> IO Int64
getIdForRow h table col val = do
  existingId <- query h selectSQL (Only val)
  case existingId of
    (Only i : _) -> return i
    _            -> execute h insertSQL (Only val)
                 >> lastInsertRowId h
  where
    selectSQL = mkQuery $ "select Id from " ++ table ++ " where " ++ col ++ " = ?"
    insertSQL = mkQuery $ "insert into " ++ table ++ " (" ++ col ++ ") values (?)"

mkQuery :: String -> Query
mkQuery = fromString

getAllSourceFiles :: DBHandle -> IO [CommandInfo]
getAllSourceFiles h = query_ h sql
  where sql = "select File, P.Path, W.Path, C.Command, A.Args, LastBuilt   \
              \ from SourceFiles                                           \
              \ join Paths as P on SourceFiles.Path = P.Id                 \
              \ join Paths as W on SourceFiles.WorkingDirectory = W.Id     \
              \ join BuildCommands as C on SourceFiles.BuildCommand = C.Id \
              \ join BuildArgs as A on SourceFiles.BuildArgs = A.Id"

getSourceFile :: DBHandle -> FilePath -> IO (Maybe CommandInfo)
getSourceFile h f = do
    row <- query h sql (normalise . takeFileName $ f, normalise . takeDirectory $ f)
    return $ case row of
              (ci : _) -> Just ci
              _        -> Nothing
  where sql = "select File, P.Path, W.Path, C.Command, A.Args, LastBuilt   \
              \ from SourceFiles                                           \
              \ join Paths as P on SourceFiles.Path = P.Id                 \
              \ join Paths as W on SourceFiles.WorkingDirectory = W.Id     \
              \ join BuildCommands as C on SourceFiles.BuildCommand = C.Id \
              \ join BuildArgs as A on SourceFiles.BuildArgs = A.Id        \
              \ where File = ? and P.Path = ? limit 1"

-- Eventually this should be more statistical, but right now it will just
-- return an arbitrary file from the same directory.
getSimilarSourceFile :: DBHandle -> FilePath -> IO (Maybe CommandInfo)
getSimilarSourceFile h f = do
    row <- query h sql (Only $ takeDirectory f)
    return $ case row of
              (ci : _) -> Just ci
              _        -> Nothing
  where sql = "select File, P.Path, W.Path, C.Command, A.Args, LastBuilt   \
              \ from SourceFiles                                           \
              \ join Paths as P on SourceFiles.Path = P.Id                 \
              \ join Paths as W on SourceFiles.WorkingDirectory = W.Id     \
              \ join BuildCommands as C on SourceFiles.BuildCommand = C.Id \
              \ join BuildArgs as A on SourceFiles.BuildArgs = A.Id        \
              \ where P.Path = ? limit 1"

-- Checks that the database has the correct schema and sets it up if needed.
ensureSchema :: DBHandle -> IO ()
ensureSchema h = defineMetadataTable h
              >> definePathsTable h
              >> defineBuildCommandsTable h
              >> defineBuildArgsTable h
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
