module Pygmalion.Core
( CommandInfo (..)
, SourceFile
, WorkingDirectory
, Command
, updateSourceFile
, queryExecutable
, scanExecutable
, makeExecutable
, clangExecutable
, clangppExecutable
) where

import Data.Int

-- The information we collect about a compilation command.
data CommandInfo = CommandInfo SourceFile WorkingDirectory Command Time
type SourceFile = String
type WorkingDirectory = String
type Command = [String]
type Time = Int64

updateSourceFile :: CommandInfo -> SourceFile -> CommandInfo
updateSourceFile (CommandInfo sf wd cmd t) sf' = CommandInfo sf' wd cmd t

-- Executable names.
queryExecutable = "pygmalion"
scanExecutable = "pygscan"
makeExecutable = "pygmake"
clangExecutable = "clang"
clangppExecutable = "clang++"
