{-# LANGUAGE DeriveGeneric #-}

module Pygmalion.RPC.Request
( RPCRequest (..)
, RPCResponse (..)
) where

import Data.Serialize
import qualified Data.Vector as V
import Data.Vector.Serialize ()
import GHC.Generics

import Pygmalion.Core
import Pygmalion.Database.Request

data RPCRequest = RPCIndexCommand CommandInfo Time
                | RPCIndexFile SourceFile Time
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
                | RPCFoundUpdates (V.Vector DBUpdate)
                | RPCUpdateAndFindDirtyInclusions SourceFileHash [Inclusion]
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
