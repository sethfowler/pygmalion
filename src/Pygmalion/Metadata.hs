module Pygmalion.Metadata
( CommandInfo (..)
, SourceFile
, WorkingDirectory
, Command
, updateSourceFile
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
