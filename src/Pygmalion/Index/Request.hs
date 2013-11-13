module Pygmalion.Index.Request
( IndexRequest (..)
, reqSF
, combineReqs
) where

import Pygmalion.Core

data IndexRequest = FromBuild     CommandInfo
                  | FromDepChange CommandInfo Time
                  | FromNotify    SourceFile
                    deriving (Show)

reqSF :: IndexRequest -> SourceFile
reqSF (FromBuild  ci)      = ciSourceFile ci
reqSF (FromNotify sf)      = sf
reqSF (FromDepChange ci _) = ciSourceFile ci

-- Combines two IndexRequests. Assumes that the first argument is 'new'
-- and the second argument is 'old'.
combineReqs :: IndexRequest -> IndexRequest -> IndexRequest
combineReqs (FromDepChange _ t) (FromBuild ci) = FromDepChange ci t
combineReqs new@(FromDepChange _ _) _          = new
combineReqs (FromBuild ci) (FromDepChange _ t) = FromDepChange ci t
combineReqs new@(FromBuild _) _                = new
combineReqs (FromNotify _) old                 = old
