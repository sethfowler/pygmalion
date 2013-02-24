module Pygmalion.Metadata
( CommandInfo (..)
, SourceFile
, WorkingDirectory
, Command
) where

import Data.Int

-- The information we collect about a compilation command.
data CommandInfo = CommandInfo SourceFile WorkingDirectory Command Time
type SourceFile = String
type WorkingDirectory = String
type Command = [String]
type Time = Int64
