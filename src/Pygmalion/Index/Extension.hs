{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Index.Extension
( hasSourceExtension
, hasHeaderExtension
, hasSourceExtensionBS
, hasHeaderExtensionBS
, hasCExtension
, hasCPPExtension
) where

import qualified Data.ByteString as B
import Data.List

sourceExtensions, headerExtensions :: [String]
sourceExtensions = [".c", ".cc", ".cpp", ".C"]
headerExtensions = [".h", ".hh", ".hpp", ".H", ".inc"]

sourceExtensionsBS, headerExtensionsBS :: [B.ByteString]
sourceExtensionsBS = [".c", ".cc", ".cpp", ".C"]
headerExtensionsBS = [".h", ".hh", ".hpp", ".H", ".inc"]

hasSourceExtension, hasHeaderExtension :: String -> Bool
hasSourceExtension f = any (`isSuffixOf` f) sourceExtensions
hasHeaderExtension f = any (`isSuffixOf` f) headerExtensions

hasSourceExtensionBS, hasHeaderExtensionBS :: B.ByteString -> Bool
hasSourceExtensionBS f = any (`B.isSuffixOf` f) sourceExtensionsBS
hasHeaderExtensionBS f = any (`B.isSuffixOf` f) headerExtensionsBS

cExtensions, cppExtensions :: [String]
cExtensions   = [".c", ".h"]
cppExtensions = [".cc", ".cpp", ".C", ".hh", ".hpp", ".H"]

hasCExtension, hasCPPExtension :: String -> Bool
hasCExtension f   = any (`isSuffixOf` f) cExtensions
hasCPPExtension f = any (`isSuffixOf` f) cppExtensions
