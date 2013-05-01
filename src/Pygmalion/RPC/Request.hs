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
                | RPCGetBases USR
                | RPCGetOverrides USR
                | RPCGetRefs USR
                | RPCGetReferenced SourceLocation
                | RPCFoundDef DefInfo
                | RPCFoundOverride Override
                | RPCFoundRef Reference
                | RPCFoundInclusion Inclusion
                | RPCPing
                | RPCLog String
                | RPCDone
                deriving (Eq, Show, Generic)

instance Serialize RPCRequest

data RPCResponse a = RPCOK a
                   | RPCError
                   deriving (Eq, Show, Generic)

instance Serialize a => Serialize (RPCResponse a)
