{-# LANGUAGE RecordWildCards #-}

module Pygmalion.Dot
( Graph (..)
, Node (..)
, Edge (..)
, asDot
, reverseEdge
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import Data.Tuple.Curry (uncurryN)
import Text.Dot ((.->.), NodeId, showDot, userNode, userNodeId)

data Graph = Graph
  { graphNodes :: [Node]
  , graphEdges :: [Edge]
  } deriving (Eq, Show)

data Node = Node
  { nodeId    :: Int
  , nodeName  :: B.ByteString
  , nodeAttrs :: [(String, String)]
  } deriving (Eq, Show)

data Edge = Edge
  { edgeFrom :: Int
  , edgeTo   :: Int
  } deriving (Eq, Show)

reverseEdge :: Int -> Int -> Edge
reverseEdge a b = Edge b a

asDotNode :: Node -> (NodeId, [(String, String)])
asDotNode Node {..} = (userNodeId nodeId, ("label", BU.toString nodeName) : nodeAttrs)

asDotEdge :: Edge -> (NodeId, NodeId)
asDotEdge Edge {..} = (userNodeId edgeFrom, userNodeId edgeTo)

asDot :: Graph -> String
asDot g = showDot $ do
  mapM_ (uncurryN userNode . asDotNode) (graphNodes g)
  mapM_ (uncurryN (.->.) . asDotEdge) (graphEdges g)
