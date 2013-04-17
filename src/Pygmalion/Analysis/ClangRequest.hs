{-# LANGUAGE DeriveGeneric #-}

module Pygmalion.Analysis.ClangRequest
( ClangRequest (..)
, ClangResponse (..)
) where

import Data.Serialize
import GHC.Generics

import Pygmalion.Core

data ClangRequest = Analyze CommandInfo
                  | Shutdown
                  deriving (Eq, Show, Generic)

instance Serialize ClangRequest

data ClangResponse = FoundDef DefInfo
                   | EndOfDefs
                   deriving (Eq, Show, Generic)

instance Serialize ClangResponse
