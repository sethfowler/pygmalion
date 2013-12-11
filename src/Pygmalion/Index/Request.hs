module Pygmalion.Index.Request
( IndexRequest (..)
, indexRequestForCommand
, indexRequestForUpdate
, indexRequestForDepChange
, combineReqs
) where

import Control.Applicative

import Pygmalion.Core

data IndexRequest = IndexRequest
  { irFile        :: !SourceFile
  , irCommandInfo :: !(Maybe CommandInfo)
  , irMTime       :: !(Maybe Time)
  , irVersionHash :: !(Maybe TimeHash)
  } deriving (Show)

-- Smart constructors.
indexRequestForCommand :: CommandInfo -> Time -> IndexRequest
indexRequestForCommand ci mtime = IndexRequest (ciSourceFile ci) (Just ci) (Just mtime) Nothing

indexRequestForUpdate :: SourceFile -> Time -> IndexRequest
indexRequestForUpdate sf mtime = IndexRequest sf Nothing (Just mtime) Nothing

indexRequestForDepChange :: SourceFile -> TimeHash -> IndexRequest
indexRequestForDepChange sf vh = IndexRequest sf Nothing Nothing (Just vh)

-- Combines two IndexRequests. Assumes that the first argument is 'new'
-- and the second argument is 'old'.
combineReqs :: IndexRequest -> IndexRequest -> IndexRequest
combineReqs (IndexRequest f newCI newMTime newVH)
            (IndexRequest _ oldCI oldMTime oldVH)
  = IndexRequest f (newCI <|> oldCI) (newMTime <|> oldMTime) (newVH <|> oldVH)
