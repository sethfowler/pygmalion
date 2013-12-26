{-# LANGUAGE DeriveGeneric #-}

module Pygmalion.Database.Request
( DBUpdate (..)
, DBRequest (..)
, DBUpdateChan
, DBQueryChan
, newDBUpdateChan
, pushUpdate
, popUpdates
, isEmptyUpdateChan
) where

import Control.Applicative
import Control.Concurrent.STM
import Data.Serialize
import Data.Vector
import GHC.Generics

import Control.Concurrent.Chan.Len
import Pygmalion.Core

data DBUpdate = DBUpdateDef !DefUpdate
              | DBUpdateOverride !Override
              | DBUpdateRef !ReferenceUpdate
              | DBUpdateCommandInfo !CommandInfo
              | DBUpdateFile !SourceFile !Time !TimeHash
              | DBUpdateInclusion !Inclusion
              | DBResetMetadata !SourceFile
                deriving (Eq, Generic, Show)

instance Serialize DBUpdate
                         
data DBRequest = DBGetCommandInfo !SourceFile (Response (Maybe CommandInfo, Maybe Time))
               | DBGetSimilarCommandInfo !SourceFile (Response (Maybe CommandInfo))
               | DBGetDefinition !SourceLocation (Response [DefInfo])
               | DBGetInclusions !SourceFile (Response [SourceFile])
               | DBGetIncluders !SourceFile (Response [SourceFile])
               | DBGetDirectIncluders !SourceFile (Response [SourceFile])
               | DBGetInclusionHierarchy !SourceFile (Response String)
               | DBGetCallers !SourceLocation (Response [Invocation])
               | DBGetCallees !SourceLocation (Response [DefInfo])
               | DBGetBases !SourceLocation (Response [DefInfo])
               | DBGetOverrides !SourceLocation (Response [DefInfo])
               | DBGetMembers !SourceLocation (Response [DefInfo])
               | DBGetRefs !SourceLocation (Response [SourceReference])
               | DBGetReferenced !SourceLocation (Response (Maybe SourceReferenced))
               | DBGetDeclReferenced !SourceLocation (Response [DefInfo])
               | DBGetHierarchy !SourceLocation (Response String)
               | DBCommitStagedUpdates (Response ())
               | DBShutdown
                 deriving (Show)

dbQueueLimit :: Int
dbQueueLimit = 5000 -- 100000

data DBUpdateChan = DBUpdateChan
  { duQueue :: TVar [Vector DBUpdate]
  , duLen   :: TVar Int
  }

newDBUpdateChan :: IO DBUpdateChan
newDBUpdateChan = DBUpdateChan <$> newTVarIO []
                               <*> newTVarIO 0

pushUpdate :: DBUpdateChan -> Vector DBUpdate -> IO ()
pushUpdate chan up = atomically $ do
  n <- readTVar (duLen chan)
  check (n < dbQueueLimit)
  q <- readTVar (duQueue chan)
  writeTVar (duLen chan) $! (n + 1)
  writeTVar (duQueue chan) $! (up : q)

popUpdates :: DBUpdateChan -> STM [Vector DBUpdate]
popUpdates chan = do
  n <- readTVar (duLen chan)
  check (n > 0)
  q <- readTVar (duQueue chan)
  writeTVar (duLen chan) 0
  writeTVar (duQueue chan) []
  return q

isEmptyUpdateChan :: DBUpdateChan -> STM Bool
isEmptyUpdateChan chan = do
  n <- readTVar (duLen chan)
  return $! n == 0

--type DBUpdateChan = LenChan (Vector DBUpdate)
type DBQueryChan = LenChan DBRequest
