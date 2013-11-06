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
, addEdge
, asDot
) where

import qualified Data.ByteString as B
import qualified Data.Graph.Inductive as G
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as E

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

-- Ignores duplicate edges.
addEdge :: Edge -> Graph -> Graph
addEdge e@(a, b, _) g = if b `elem` G.suc g a then g
                                              else G.insEdge e g

bsToText :: B.ByteString -> TL.Text
bsToText = TL.fromStrict . E.decodeUtf8

dotAttribsForNode :: Node -> Attributes
dotAttribsForNode (_, NodeLabel {..}) = highlight nodeHighlight $ label nodeName nodeItems
  where
    label n is = [toLabel $ FieldLabel (bsToText n) : map label' is]
    label' is  = FieldLabel $ TL.concat $ map ((`TL.append` "\\l") . bsToText) is

    highlight True as  = PenWidth 4.0 : as
    highlight False as = as

asDot :: Graph -> String
asDot = TL.unpack . printDotGraph . graphToDot params
  where
    params = nonClusteredParams { globalAttributes = [ GraphAttrs [RankDir FromLeft]
                                                     , NodeAttrs [Shape MRecord]
                                                     , EdgeAttrs [Color [toWC $ X11Color Gray]]]
                                , isDirected       = True
                                , fmtNode          = dotAttribsForNode
                                }
