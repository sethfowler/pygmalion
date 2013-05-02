{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Analysis.Extension
( hasSourceExtension
, hasHeaderExtension
, hasCExtension
, hasCPPExtension
) where

import Data.List

sourceExtensions, headerExtensions :: [String]
sourceExtensions = [".c", ".cc", ".cpp", ".C"]
headerExtensions = [".h", ".hh", ".hpp", ".H"]

hasSourceExtension, hasHeaderExtension :: String -> Bool
hasSourceExtension f = any (`isSuffixOf` f) sourceExtensions
hasHeaderExtension f = any (`isSuffixOf` f) headerExtensions

cExtensions, cppExtensions :: [String]
cExtensions   = [".c", ".h"]
cppExtensions = [".cc", ".cpp", ".C", ".hh", ".hpp", ".H"]

hasCExtension, hasCPPExtension :: String -> Bool
hasCExtension f   = any (`isSuffixOf` f) cExtensions
hasCPPExtension f = any (`isSuffixOf` f) cppExtensions
