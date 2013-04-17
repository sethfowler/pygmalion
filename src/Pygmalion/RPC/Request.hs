{-# LANGUAGE DeriveGeneric #-}

module Pygmalion.RPC.Request
( RPCRequest (..)
) where

import Data.Serialize
import GHC.Generics

import Pygmalion.Core

data RPCRequest = RPCSendCommandInfo CommandInfo
                | RPCGetCommandInfo SourceFile
                | RPCGetSimilarCommandInfo SourceFile
                | RPCGetDefinition USR
                deriving (Eq, Show, Generic)

instance Serialize RPCRequest
