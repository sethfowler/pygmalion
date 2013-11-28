module Pygmalion.Index.Request
( IndexRequest (..)
, reqSF
, combineReqs
) where

import Pygmalion.Core

data IndexRequest = IndexAdd     !CommandInfo
                  | IndexUpdate  !SourceFile
                    deriving (Show)

reqSF :: IndexRequest -> SourceFile
reqSF (IndexAdd  ci)   = ciSourceFile ci
reqSF (IndexUpdate sf) = sf

-- Combines two IndexRequests. Assumes that the first argument is 'new'
-- and the second argument is 'old'.
combineReqs :: IndexRequest -> IndexRequest -> IndexRequest
combineReqs new@(IndexAdd _) _  = new
combineReqs (IndexUpdate _) old = old
