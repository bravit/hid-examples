{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphs where

import Data.Map (Map)
import qualified Data.Map as Map (fromList)

class Graph g where
  type Vertex g -- associated type family
  data Edge g   -- associated data family
  src, tgt :: Edge g -> Vertex g
  outEdges :: g -> Vertex g -> [Edge g]
  -- other methods

neighbours :: Graph g => g -> Vertex g -> [Vertex g]
neighbours g v = map tgt (outEdges g v)

isLoop :: (Graph g, Eq (Vertex g)) => g -> Edge g -> Bool
isLoop _ e = src e == tgt e


-- Representation #1: list of edges
newtype EdgesList = EdgesList [Edge EdgesList]

instance Graph EdgesList where
  type Vertex EdgesList = Int
  data Edge EdgesList = MkEdge1 (Vertex EdgesList) (Vertex EdgesList)
  src = undefined
  tgt = undefined
  outEdges = undefined

g1 :: EdgesList
g1 = EdgesList [MkEdge1 0 1, MkEdge1 1 0]


-- Representation #2: lists of adjacent vertices
newtype VertexMap = VertexMap (Map (Vertex VertexMap) [Vertex VertexMap])

instance Graph VertexMap where
  type Vertex VertexMap = String
  data Edge VertexMap = MkEdge2 Int (Vertex VertexMap)
                             (Vertex VertexMap)
  src = undefined
  tgt = undefined
  outEdges = undefined

g2 :: VertexMap
g2 = VertexMap (Map.fromList [("A", ["B"]), ("B", ["A"])])

