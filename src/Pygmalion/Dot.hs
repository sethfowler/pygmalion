{-# LANGUAGE RecordWildCards #-}

module Pygmalion.Dot
( Graph (..)
, Node (..)
, Edge (..)
, mkNode
, highlightNode
, asDot
, reverseEdge
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import Data.Set (Set, toList)
import Data.Tuple.Curry (uncurryN)
import Text.Dot (attribute, edge, NodeId, showDot, userNode, userNodeId)

data Graph = Graph
  { graphNodes :: Set Node
  , graphEdges :: Set Edge
  } deriving (Eq, Show)

data Node = Node
  { nodeId    :: Int
  , nodeName  :: B.ByteString
  , nodeItems :: [[B.ByteString]]
  , nodeAttrs :: [(String, String)]
  } deriving (Eq, Ord, Show)

data Edge = Edge
  { edgeFrom :: Int
  , edgeTo   :: Int
  } deriving (Eq, Ord, Show)

mkNode :: Int -> B.ByteString -> [[B.ByteString]] -> Node
mkNode nid name items = Node nid name items [("shape", "record")]

highlightNode :: Node -> Node
highlightNode node = node { nodeAttrs = ("penwidth", "4") : nodeAttrs node }

reverseEdge :: Int -> Int -> Edge
reverseEdge a b = Edge b a

dotLabel :: Node -> String
dotLabel Node {..} = "{" ++ BU.toString nodeName ++ concatMap toSection nodeItems ++ "}"
  where
    toSection :: [B.ByteString] -> String
    toSection items = '|' : concatMap ((++ "\\1") . BU.toString) items

asDotNode :: Node -> (NodeId, [(String, String)])
asDotNode node@Node {..} = (userNodeId nodeId, ("label", dotLabel node) : nodeAttrs)

asDotEdge :: Edge -> (NodeId, NodeId, [(String, String)])
asDotEdge Edge {..} = (userNodeId edgeFrom, userNodeId edgeTo, [("color", "grey")])

asDot :: Graph -> String
asDot g = showDot $ do
  attribute ("rankdir", "LR")
  mapM_ (uncurryN userNode . asDotNode) (toList . graphNodes $ g)
  mapM_ (uncurryN edge . asDotEdge) (toList . graphEdges $ g)
