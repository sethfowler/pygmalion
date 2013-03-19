{-# LANGUAGE DeriveGeneric #-}

module Pygmalion.Core
( CommandInfo (..)
, SourceFile
, WorkingDirectory
, Command (..)
, Port
, withSourceFile
, queryExecutable
, scanExecutable
, makeExecutable
, dbFile
, configFile
, compileCommandsFile
) where

import Data.Int
import Data.List
import Data.Serialize
import Database.SQLite.Simple (FromRow(..), ToRow(..), field)
import Database.SQLite.Simple.ToField (ToField(..))
import GHC.Generics
import System.FilePath.Posix

type Port = Int

type SourceFile = String
type WorkingDirectory = String
type Time = Int64

-- The information we collect about a compilation command.
data CommandInfo = CommandInfo SourceFile WorkingDirectory Command Time
  deriving (Eq, Show, Generic)
instance Serialize CommandInfo

instance ToRow CommandInfo where
  toRow (CommandInfo sf wd (Command cmd args) t) =
    [toField . takeFileName $ sf,
     toField . takeDirectory $ sf,
     toField wd,
     toField (intercalate " " (cmd : args)),
     toField t]

instance FromRow CommandInfo where
  fromRow = do
    file <- field
    directory <- field
    wd <- field
    cmdAndArgs <- field
    cmd <- case words cmdAndArgs of
      (c : as) -> return $ Command c as
      _        -> error $ "Malformed row for file " ++ file
    time <- field
    return $ CommandInfo (normalise $ combine directory file) wd cmd time

data Command = Command String [String]
  deriving (Eq, Show, Generic)
instance Serialize Command

withSourceFile :: CommandInfo -> SourceFile -> CommandInfo
withSourceFile (CommandInfo _ wd cmd t) sf' = CommandInfo sf' wd cmd t

-- Tool names.
queryExecutable, scanExecutable, makeExecutable :: String
queryExecutable = "pygmalion"
scanExecutable  = "pygscan"
makeExecutable  = "pygmake"

-- Data files.
dbFile, configFile, compileCommandsFile :: String
dbFile              = ".pygmalion.sqlite"
configFile          = ".pygmalion.conf"
compileCommandsFile = "compile_commands.json"
