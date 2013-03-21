module Pygmalion.Analyze.Extension
( hasSourceExtension
, hasHeaderExtension
) where

import Data.List

sourceExtensions, headerExtensions :: [String]
sourceExtensions = [".c", ".cc", ".cpp", ".C"]
headerExtensions = [".h", ".hh", ".hpp", ".H"]

hasSourceExtension, hasHeaderExtension :: String -> Bool
hasSourceExtension a = any (`isSuffixOf` a) sourceExtensions
hasHeaderExtension a = any (`isSuffixOf` a) headerExtensions
