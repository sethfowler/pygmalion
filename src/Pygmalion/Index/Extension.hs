{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}

module Pygmalion.Index.Extension
( ExtensionKind (..)
, extensionKind
, hasSourceExtension
, extensionLanguage
) where

import qualified Data.ByteString as B
import Data.List

import Pygmalion.Core

data ExtensionKind = SourceExtension
                   | HeaderExtension
                   | UnknownExtension
                     deriving (Show)

class ExtensionKindable a where
  extensionKind      :: a -> ExtensionKind
  hasSourceExtension :: a -> Bool

instance ExtensionKindable String where
  extensionKind f | any (`isSuffixOf` f) sourceExtensions = SourceExtension
                  | any (`isSuffixOf` f) headerExtensions = HeaderExtension
                  | otherwise                             = UnknownExtension
  hasSourceExtension f | any (`isSuffixOf` f) sourceExtensions = True
                       | otherwise                             = False

instance ExtensionKindable B.ByteString where
  extensionKind f | any (`B.isSuffixOf` f) sourceExtensionsBS = SourceExtension
                  | any (`B.isSuffixOf` f) headerExtensionsBS = HeaderExtension
                  | otherwise                                 = UnknownExtension
  hasSourceExtension f | any (`B.isSuffixOf` f) sourceExtensionsBS = True
                       | otherwise                                 = False

sourceExtensions, headerExtensions :: [String]
sourceExtensions = [".c", ".cc", ".cpp", ".C"]
headerExtensions = [".h", ".hh", ".hpp", ".H", ".inc"]

sourceExtensionsBS, headerExtensionsBS :: [B.ByteString]
sourceExtensionsBS = [".c", ".cc", ".cpp", ".C"]
headerExtensionsBS = [".h", ".hh", ".hpp", ".H", ".inc"]

extensionLanguage :: String -> Language
extensionLanguage f | any (`isSuffixOf` f) cExtensions   = CLanguage
                    | any (`isSuffixOf` f) cppExtensions = CPPLanguage
                    | otherwise                          = UnknownLanguage

cExtensions, cppExtensions :: [String]
cExtensions   = [".c", ".h"]
cppExtensions = [".cc", ".cpp", ".C", ".hh", ".hpp", ".H"]
