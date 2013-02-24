module SourceDB
( withDB
, updateRecord
, DBHandle
) where

import Control.Exception
import Control.Monad
import Data.List
import Data.Int
import Database.SQLite

import Metadata

-- Configuration.
dbFilename = ".pygmalion.sqlite"

-- Schema for the database.
dbInt = SQLInt NORMAL True True
dbString = SQLVarChar 2048
dbPath = SQLVarChar 2048

dbToolName = "pygmalion"

dbMajorVersion :: Int64
dbMajorVersion = 0

dbMinorVersion :: Int64
dbMinorVersion = 1

metadataTable :: SQLTable
metadataTable = Table "Metadata"
                [
                  Column "Tool" dbString [Unique],
                  Column "MajorVersion" dbInt [],
                  Column "MinorVersion" dbInt []
                ] []
execGetDBVersion h = execParamStatement h sql params
  where sql = "select MajorVersion, MinorVersion from Metadata " ++
              "where Tool = :tool"
        params = [(":tool", Text dbToolName)]
execSetDBVersion h = execParamStatement_ h sql params
  where sql = "insert into Metadata (Tool, MajorVersion, MinorVersion) " ++
              "values (:tool, :major, :minor)"
        params = [(":tool",  Text dbToolName),
                  (":major", Int dbMajorVersion),
                  (":minor", Int dbMinorVersion)]

sourceFileTable :: SQLTable
sourceFileTable = Table "SourceFiles"
                  [
                    Column "File" dbPath [Unique],
                    Column "WorkingDirectory" dbPath [],
                    Column "Command" dbString [],
                    Column "LastBuilt" dbInt []
                  ] []
execSourceFileUpdate h (CommandInfo file wd cmd time) =
  execParamStatement_ h sql params
  where sql =  "replace into SourceFiles "
            ++ "(File, WorkingDirectory, Command, LastBuilt) "
            ++ "values (:file, :wd, :cmd, :time)"
        params = [(":file", Text file),
                  (":wd",   Text wd),
                  (":cmd",  Text $ intercalate " " cmd),
                  (":time", Int time)]

schema = [metadataTable, sourceFileTable]

-- Database manipulation functions.
type DBHandle = SQLiteHandle

withDB :: (DBHandle -> IO a) -> IO a
withDB = bracket openDB closeDB

openDB :: IO DBHandle
openDB = do handle <- openConnection dbFilename
            ensureSchema handle
            return handle

closeDB :: DBHandle -> IO ()
closeDB = closeConnection

updateRecord :: DBHandle -> CommandInfo -> IO ()
updateRecord h ci = execSourceFileUpdate h ci >>= ensureNothing

-- Checks that the database has the correct schema and sets it up if needed.
ensureSchema :: DBHandle -> IO ()
ensureSchema h = forM_ schema $ \table -> defineTableOpt h True table
                                      >>= ensureNothing
                                      >>  ensureVersion h

ensureVersion :: DBHandle -> IO ()
ensureVersion h = execGetDBVersion h >>= ensureRight >>= checkVersion
  where
    checkVersion [[[("MajorVersion", Int dbMajorVersion),
                    ("MinorVersion", Int dbMinorVersion)]]] = return ()
    checkVersion [[]] = execSetDBVersion h >>= ensureNothing
    checkVersion rs = throwDBVersionError rs

throwDBVersionError :: Show a => [[Row a]] -> IO ()
throwDBVersionError rs = error $ "Database version must be "
                             ++ (show dbMajorVersion) ++ "."
                             ++ (show dbMinorVersion) ++ " but I got "
                             ++ (show rs)

-- Utility functions.
ensureNothing :: Maybe String -> IO ()
ensureNothing (Just s) = error s
ensureNothing _        = return ()

ensureRight :: Either String a -> IO a
ensureRight (Left s)  = error s
ensureRight (Right a) = return a
