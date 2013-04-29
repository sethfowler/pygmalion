{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Analysis.Extension
( hasSourceExtension
, hasHeaderExtension
, hasHeaderExtensionText
, hasCExtension
, hasCPPExtension
) where

import Data.List
import qualified Data.Text as T

sourceExtensions, headerExtensions :: [String]
sourceExtensions = [".c", ".cc", ".cpp", ".C"]
headerExtensions = [".h", ".hh", ".hpp", ".H"]

hasSourceExtension, hasHeaderExtension :: String -> Bool
hasSourceExtension f = any (`isSuffixOf` f) sourceExtensions
hasHeaderExtension f = any (`isSuffixOf` f) headerExtensions

headerExtensionsText :: [T.Text]
headerExtensionsText = [".h", ".hh", ".hpp", ".H"]

hasHeaderExtensionText :: T.Text -> Bool
hasHeaderExtensionText f = any (`T.isSuffixOf` f) headerExtensionsText

cExtensions, cppExtensions :: [String]
cExtensions   = [".c", ".h"]
cppExtensions = [".cc", ".cpp", ".C", ".hh", ".hpp", ".H"]

hasCExtension, hasCPPExtension :: String -> Bool
hasCExtension f   = any (`isSuffixOf` f) cExtensions
hasCPPExtension f = any (`isSuffixOf` f) cppExtensions
