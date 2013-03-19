module Pygmalion.Database
( ensureDB
, withDB
, updateRecord
, getAllRecords
, DBHandle
) where

import Prelude hiding (catch)
import Control.Exception(bracket, catch, SomeException)
import Data.List
import Data.Int
import Database.SQLite
import System.FilePath.Posix -- TODO: Remove this dependency.

import Pygmalion.Core

-- TODO: Switch to direct-sqlite and simple-simple. Database.SQLite is garbage.
-- I could eliminate a great deal of this code by switching.

-- Schema for the database.
dbInt, dbString {-, dbPath -} :: SQLType
dbInt = SQLInt NORMAL True True
dbString = SQLVarChar 2048
{- dbPath = SQLVarChar 2048 -}

dbToolName :: String
dbToolName = "pygmalion"

dbMajorVersion, dbMinorVersion :: Int64
dbMajorVersion = 0
dbMinorVersion = 2

metadataTable :: SQLTable
metadataTable = Table "Metadata"
                [
                  Column "Tool" dbString [PrimaryKey False, IsNullable False],
                  Column "MajorVersion" dbInt [IsNullable False],
                  Column "MinorVersion" dbInt [IsNullable False]
                ] []

execGetDBVersion :: SQLiteResult a => SQLiteHandle -> IO (Either String [[Row a]])
execGetDBVersion h = execParamStatement h sql params
  where sql = "select MajorVersion, MinorVersion from Metadata " ++
              "where Tool = :tool"
        params = [(":tool", Text dbToolName)]

execSetDBVersion :: SQLiteHandle -> IO (Maybe String)
execSetDBVersion h = execParamStatement_ h sql params
  where sql = "insert into Metadata (Tool, MajorVersion, MinorVersion) " ++
              "values (:tool, :major, :minor)"
        params = [(":tool",  Text dbToolName),
                  (":major", Int dbMajorVersion),
                  (":minor", Int dbMinorVersion)]

{-
sourceFileTable :: SQLTable
sourceFileTable = Table "SourceFiles"
                  [
                    Column "File" dbPath [IsNullable False],
                    Column "Directory" dbPath [IsNullable False],
                    Column "WorkingDirectory" dbPath [IsNullable False],
                    Column "Command" dbString [IsNullable False],
                    Column "LastBuilt" dbInt [IsNullable False]
                  ] [TablePrimaryKey ["File", "Directory"]]
-}

-- We need to define SourceFiles manually because Database.SQLite doesn't
-- support table constraints.
defineSourceFiles :: SQLiteHandle -> IO (Maybe String)
defineSourceFiles h = execStatement_ h sql
  where sql =  "create table if not exists SourceFiles("
            ++ "File varchar(2048) not null,"
            ++ "Directory varchar(2048) not null,"
            ++ "WorkingDirectory varchar(2048) not null,"
            ++ "Command varchar(2048) not null,"
            ++ "LastBuilt integer zerofill unsigned not null,"
            ++ "primary key(File, Directory))"

execUpdateSourceFile :: SQLiteHandle -> CommandInfo -> IO (Maybe String)
execUpdateSourceFile h (CommandInfo file wd (Command cmd args) time) =
    execParamStatement_ h sql params
  where sql =  "replace into SourceFiles "
            ++ "(File, Directory, WorkingDirectory, Command, LastBuilt) "
            ++ "values (:file, :dir, :wd, :cmd, :time)"
        params = [(":file", Text (takeFileName file)),
                  (":dir",  Text (takeDirectory file)),
                  (":wd",   Text wd),
                  (":cmd",  Text $ intercalate " " (cmd : args)),
                  (":time", Int time)]

execGetAllSourceFiles :: SQLiteResult a => SQLiteHandle
                                        -> IO (Either String [[Row a]])
execGetAllSourceFiles h = execStatement h sql
  where sql =  "select ((case when Directory == '.' "
                          ++ "then '' "
                          ++ "else Directory || '/' "
                     ++ "end) || File) as file, "
            ++ "WorkingDirectory as directory, "
            ++ "Command as command "
            ++ "from SourceFiles"

-- Debugging functions.
withCatch :: String -> IO a -> IO a
withCatch lbl f = catch f printAndRethrow
  where printAndRethrow :: SomeException -> IO a
        printAndRethrow e = error $ lbl ++ ": " ++ (show e)

-- Database manipulation functions.
type DBHandle = SQLiteHandle

ensureDB :: FilePath -> IO ()
ensureDB db = withDB db (const . return $ ())

withDB :: FilePath -> (DBHandle -> IO a) -> IO a
withDB db f = bracket (withCatch "open" (openDB db)) closeDB f

openDB :: FilePath -> IO DBHandle
openDB db = do
  handle <- openConnection db
  ensureSchema handle
  return handle

closeDB :: DBHandle -> IO ()
closeDB = closeConnection

updateRecord :: DBHandle -> CommandInfo -> IO ()
updateRecord h ci = withCatch "updateRecord" $ execUpdateSourceFile h ci >>= ensureNothing

getAllRecords :: DBHandle -> IO [Row Value]
getAllRecords h = withCatch "getAllRecords" $ execGetAllSourceFiles h >>= ensureRight >>= return . concat

-- Checks that the database has the correct schema and sets it up if needed.
ensureSchema :: DBHandle -> IO ()
ensureSchema h = do
      defineTableOpt h True metadataTable >>= ensureNothing
      -- We need to define SourceFiles manually because Database.SQLite doesn't
      -- support table constraints.
      defineSourceFiles h >>= ensureNothing
      ensureVersion h

ensureVersion :: DBHandle -> IO ()
ensureVersion h = execGetDBVersion h >>= ensureRight >>= checkVersion
  where
    checkVersion [[[("MajorVersion", Int major),
                    ("MinorVersion", Int minor)]]]
                 | (major, minor) == (dbMajorVersion, dbMinorVersion) = return ()
                 | otherwise = throwDBVersionError major minor
    checkVersion [[]] = execSetDBVersion h >>= ensureNothing
    checkVersion _    = error $ "Couldn't read database version"

throwDBVersionError :: Int64 -> Int64 -> IO ()
throwDBVersionError major minor  =  error $ "Database version "
                                 ++ (show major) ++ "." ++ (show minor)
                                 ++ " is different than required version "
                                 ++ (show dbMajorVersion) ++ "."
                                 ++ (show dbMinorVersion)

-- Utility functions.
ensureNothing :: Maybe String -> IO ()
ensureNothing (Just s) = error s
ensureNothing _        = return ()

ensureRight :: Either String a -> IO a
ensureRight (Left s)  = error s
ensureRight (Right a) = return a
