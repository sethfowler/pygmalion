{-# LANGUAGE DeriveGeneric #-}

module Pygmalion.RPC.Request
( RPCRequest (..)
, RPCResponse (..)
) where

import Data.Serialize
import GHC.Generics

import Pygmalion.Core

data RPCRequest = RPCIndexCommand CommandInfo
                | RPCIndexFile SourceFile
                | RPCGetCommandInfo SourceFile
                | RPCGetSimilarCommandInfo SourceFile
                | RPCGetDefinition SourceLocation
                | RPCGetCallers SourceLocation
                | RPCGetCallees SourceLocation
                | RPCGetBases SourceLocation
                | RPCGetOverrides SourceLocation
                | RPCGetMembers SourceLocation
                | RPCGetRefs SourceLocation
                | RPCGetReferenced SourceLocation
                | RPCGetDeclReferenced SourceLocation
                | RPCGetHierarchy SourceLocation
                | RPCGetInclusions SourceFile
                | RPCGetIncluders SourceFile
                | RPCGetInclusionHierarchy SourceFile
                | RPCFoundDef DefUpdate
                | RPCFoundOverride Override
                | RPCFoundRef ReferenceUpdate
                | RPCFoundInclusion Inclusion
                | RPCWait
                | RPCPing
                | RPCLog String
                | RPCDone
                | RPCStop
                deriving (Eq, Show, Generic)

instance Serialize RPCRequest

data RPCResponse a = RPCOK a
                   | RPCError
                   deriving (Eq, Show, Generic)

instance Serialize a => Serialize (RPCResponse a)
