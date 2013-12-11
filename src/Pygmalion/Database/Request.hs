module Pygmalion.Database.Request
( DBUpdate (..)
, DBRequest (..)
, DBUpdateChan
, DBQueryChan
) where

import Control.Concurrent.Chan.Len
import Pygmalion.Core

data DBUpdate = DBUpdateDef DefUpdate
              | DBUpdateOverride Override
              | DBUpdateRef ReferenceUpdate
              | DBUpdateCommandInfo CommandInfo
              | DBUpdateFile SourceFile Time TimeHash
              | DBUpdateInclusion Inclusion
              | DBResetMetadata SourceFile
                deriving (Show)
                         
data DBRequest = DBGetCommandInfo SourceFile (Response (Maybe CommandInfo, Maybe Time))
               | DBGetSimilarCommandInfo SourceFile (Response (Maybe CommandInfo))
               | DBGetDefinition SourceLocation (Response [DefInfo])
               | DBGetInclusions SourceFile (Response [SourceFile])
               | DBGetIncluders SourceFile (Response [SourceFile])
               | DBGetDirectIncluders SourceFile (Response [SourceFile])
               -- | DBGetIncluderInfo SourceFile (Response [CommandInfo])
               | DBGetInclusionHierarchy SourceFile (Response String)
               | DBGetCallers SourceLocation (Response [Invocation])
               | DBGetCallees SourceLocation (Response [DefInfo])
               | DBGetBases SourceLocation (Response [DefInfo])
               | DBGetOverrides SourceLocation (Response [DefInfo])
               | DBGetMembers SourceLocation (Response [DefInfo])
               | DBGetRefs SourceLocation (Response [SourceReference])
               | DBGetReferenced SourceLocation (Response (Maybe SourceReferenced))
               | DBGetDeclReferenced SourceLocation (Response [DefInfo])
               | DBGetHierarchy SourceLocation (Response String)
               -- | DBUpdateAndFindDirtyInclusions SourceFileHash [Inclusion]
               --                                  (Response [SourceFileHash])
               | DBShutdown
                 deriving (Show)

type DBUpdateChan = LenChan [DBUpdate]
type DBQueryChan = LenChan DBRequest
