module Pygmalion.Core
( CommandInfo (..)
, SourceFile
, WorkingDirectory
, Command (..)
, Port
, commandInfoToTuple
, tupleToCommandInfo
, updateSourceFile
, queryExecutable
, scanExecutable
, makeExecutable
, clangExecutable
, clangppExecutable
, dbFile
, compileCommandsFile
) where

import Data.Int

type Port = Int

type SourceFile = String
type WorkingDirectory = String
type Time = Int64

-- The information we collect about a compilation command.
data CommandInfo = CommandInfo SourceFile WorkingDirectory Command Time
  deriving (Eq, Show)
data Command = Command String [String]
  deriving (Eq, Show)

commandInfoToTuple :: CommandInfo -> (String, String, [String], Int64)
commandInfoToTuple (CommandInfo sf wd (Command c as) t) = (sf, wd, (c : as), t)

tupleToCommandInfo :: (String, String, [String], Int64) -> Maybe CommandInfo
tupleToCommandInfo (sf, wd, (c : as), t) = Just $ CommandInfo sf wd (Command c as) t
tupleToCommandInfo _ = Nothing

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
