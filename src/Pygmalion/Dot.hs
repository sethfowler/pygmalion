{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Pygmalion.Dot
( Graph
, Node
, Edge
, mkGraph
, mkNode
, mkHighlightedNode
, mkEdge
, mkReverseEdge
, addNode
, addUniqueNode
, addEdge
, nodeElem
, asDot

-- These shouldn't really be exported, but it eliminates warnings
-- until this module can be rewritten.
, NodeLabel(..)
) where

import qualified Data.ByteString as B
--import qualified Data.ByteString.Char8 as BC
import qualified Data.Graph.Inductive as G
--import Data.GraphViz
--import Data.GraphViz.Attributes
--import qualified Data.Text.Lazy as TL
--import qualified Data.Text.Encoding as E

data NodeLabel = NodeLabel
  { nodeName      :: B.ByteString
  , nodeItems     :: [[B.ByteString]]
  , nodeHighlight :: Bool
  } deriving (Eq, Ord, Show)

type Graph = G.Gr NodeLabel ()
type Node = G.LNode NodeLabel
type Edge = G.UEdge

mkGraph :: Graph
mkGraph = G.empty

mkNode :: Int -> B.ByteString -> [[B.ByteString]] -> Node
mkNode nid name items = (nid, NodeLabel name items False)

mkHighlightedNode :: Int -> B.ByteString -> [[B.ByteString]] -> Node
mkHighlightedNode nid name items = (nid, NodeLabel name items True)

mkEdge :: Int -> Int -> Edge
mkEdge a b = (a, b, ())

mkReverseEdge :: Int -> Int -> Edge
mkReverseEdge = flip mkEdge

-- Ignores duplicate nodes.
addNode :: Node -> Graph -> Graph
addNode n@(nid, _) g = if nid `G.gelem` g then g
                                          else G.insNode n g

-- Adds a node without checking to see that it's a duplicate. By using
-- this you assert that the node you're adding is unique.
addUniqueNode :: Node -> Graph -> Graph
addUniqueNode = G.insNode

-- Ignores duplicate edges.
addEdge :: Edge -> Graph -> Graph
addEdge e@(a, b, _) g = if b `elem` G.suc g a then g
                                              else G.insEdge e g

-- Checks whether a node exists in the graph already.
nodeElem :: Int -> Graph -> Bool
nodeElem = G.gelem

{-
bsToText :: B.ByteString -> String
bsToText = BC.unpack

dotAttribsForNode :: Node -> Attributes
dotAttribsForNode (_, NodeLabel {..}) = highlight nodeHighlight $ label nodeName nodeItems
  where
    label n is = Label (StrLabel (bsToText n)) : map label' is
    label' is  = Label (StrLabel $ concat $ map ((++ "\\l") . bsToText) is)

    highlight True as  = PenWidth 4.0 : as
    highlight False as = as

asDot :: Graph -> String
asDot g = printDotGraph $ graphToDot True g params dotAttribsForNode (const [])
  where
    params = [ GraphAttrs [RankDir FromLeft]
             , NodeAttrs [Shape BoxShape]
             , EdgeAttrs [Color [ColorName "gray"]]
             ]
-}
asDot :: Graph -> String
asDot = undefined
