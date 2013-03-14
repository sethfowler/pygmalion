{-# LANGUAGE DeriveGeneric #-}

module Pygmalion.Core
( CommandInfo (..)
, SourceFile
, WorkingDirectory
, Command (..)
, Port
, updateSourceFile
, queryExecutable
, scanExecutable
, makeExecutable
, clangExecutable
, clangppExecutable
, dbFile
, compileCommandsFile
) where

--import Control.Monad
import Data.Int
import Data.Serialize
import GHC.Generics

type Port = Int

type SourceFile = String
type WorkingDirectory = String
type Time = Int64

-- The information we collect about a compilation command.
data CommandInfo = CommandInfo SourceFile WorkingDirectory Command Time
  deriving (Eq, Show, Generic)
instance Serialize CommandInfo

data Command = Command String [String]
  deriving (Eq, Show, Generic)
instance Serialize Command

updateSourceFile :: CommandInfo -> SourceFile -> CommandInfo
updateSourceFile (CommandInfo _ wd cmd t) sf' = CommandInfo sf' wd cmd t

-- Tool names.
queryExecutable, scanExecutable, makeExecutable :: String
queryExecutable = "pygmalion"
scanExecutable  = "pygscan"
makeExecutable  = "pygmake"

-- External command names.
clangExecutable, clangppExecutable :: String
clangExecutable   = "clang"
clangppExecutable = "clang++"

-- Data files.
dbFile, compileCommandsFile :: String
dbFile              = ".pygmalion.sqlite"
compileCommandsFile = "compile_commands.json"
