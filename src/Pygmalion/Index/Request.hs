module Pygmalion.Index.Request
( IndexRequest (..)
, reqSF
, combineReqs
) where

import Pygmalion.Core

-- The boolean argument indicates whether we've seen a FromNotify
-- about this file.
data IndexRequest = FromBuild     !CommandInfo !Bool
                  | FromDepChange !CommandInfo !Time !Bool
                  | FromNotify    !SourceFile
                  | FromInclusion !SourceFile
                    deriving (Show)

reqSF :: IndexRequest -> SourceFile
reqSF (FromBuild  ci _)      = ciSourceFile ci
reqSF (FromNotify sf)        = sf
reqSF (FromDepChange ci _ _) = ciSourceFile ci
reqSF (FromInclusion sf)     = sf

-- Combines two IndexRequests. Assumes that the first argument is 'new'
-- and the second argument is 'old'.
combineReqs :: IndexRequest -> IndexRequest -> IndexRequest
combineReqs (FromDepChange _ t m) (FromBuild ci m')       = FromDepChange ci t (m || m')
combineReqs (FromDepChange ci t m) (FromDepChange _ _ m') = FromDepChange ci t (m || m')
combineReqs (FromDepChange ci t _) (FromNotify _)         = FromDepChange ci t True
combineReqs (FromBuild ci m) (FromBuild _ m')             = FromBuild ci (m || m')
combineReqs (FromBuild ci m) (FromDepChange _ t m')       = FromDepChange ci t (m || m')
combineReqs (FromBuild ci _) (FromNotify _)               = FromBuild ci True
combineReqs (FromNotify _) (FromBuild ci _)               = FromBuild ci True
combineReqs (FromNotify _) (FromDepChange ci t _)         = FromDepChange ci t True
combineReqs (FromNotify _) old@(FromNotify _)             = old
combineReqs _ (FromInclusion _)                           = error "FromInclusion pending?!"
combineReqs (FromInclusion _) _                           = error "FromInclusion pending?!"
