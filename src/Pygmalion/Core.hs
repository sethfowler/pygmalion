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
updateSourceFile (CommandInfo _ wd cmd t) sf' = CommandInfo sf' wd cmd t

-- Tool names.
queryExecutable, scanExecutable, makeExecutable :: String
queryExecutable = "pygmalion"
scanExecutable = "pygscan"
makeExecutable = "pygmake"

-- External command names.
clangExecutable, clangppExecutable :: String
clangExecutable = "clang"
clangppExecutable = "clang++"
