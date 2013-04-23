{-# LANGUAGE DeriveGeneric #-}

module Pygmalion.RPC.Request
( RPCRequest (..)
, RPCResponse (..)
) where

import Data.Serialize
import GHC.Generics

import Pygmalion.Core

data RPCRequest = RPCSendCommandInfo CommandInfo
                | RPCGetCommandInfo SourceFile
                | RPCGetSimilarCommandInfo SourceFile
                | RPCGetDefinition USR
                | RPCGetCallers USR
                | RPCGetCallees USR
                | RPCGetRefs USR
                | RPCPing
                deriving (Eq, Show, Generic)

instance Serialize RPCRequest

data RPCResponse a = RPCOK a
                   | RPCError
                   deriving (Eq, Show, Generic)

instance Serialize a => Serialize (RPCResponse a)
