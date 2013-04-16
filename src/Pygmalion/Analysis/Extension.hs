{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Analysis.Extension
( hasSourceExtension
, hasHeaderExtension
, hasHeaderExtensionText
) where

import Data.List
import qualified Data.Text as T

sourceExtensions, headerExtensions :: [String]
sourceExtensions = [".c", ".cc", ".cpp", ".C"]
headerExtensions = [".h", ".hh", ".hpp", ".H"]

hasSourceExtension, hasHeaderExtension :: String -> Bool
hasSourceExtension a = any (`isSuffixOf` a) sourceExtensions
hasHeaderExtension a = any (`isSuffixOf` a) headerExtensions

headerExtensionsText :: [T.Text]
headerExtensionsText = [".h", ".hh", ".hpp", ".H"]

hasHeaderExtensionText :: T.Text -> Bool
hasHeaderExtensionText t = any (`T.isSuffixOf` t) headerExtensionsText
