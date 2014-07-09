{-|
Module      : Effusion.Genealogy
Description : Framework for Genealogy Computation
Copyright   : Travis Whitaker 2014
License     : All rights reserved.
Maintainer  : twhitak@its.jnj.com
Stability   : Provisional
Portability : POSIX

Document this.
-}

{-# LANGUAGE TypeFamilies #-}

module Effusion.Genealogy (

    -- * Types

    -- ** Tree-based Models

    Level
   ,PTree
   ,LevelFold

    -- ** Path-based Models

   ,PathLink
   ,Path
   ,PathFold

    -- * Tree-based Models
   ,pTree
   ,sinkPTrees
   ,treeDepth
   ,treeShape

    -- * Path-based Models

    -- * Graph Handling Functions
   ,isSourceNode
   ,sourceNodes
   ,isSinkNode
   ,sinkNodes
) where

import Data.List                  (concatMap)
import Data.Maybe                 (fromJust)
import Data.Graph.Inductive.Graph (Node, Graph, nodes, pre, suc, lpre, lab)

import Effusion.Data              (fastNub)

-- | One "level" of a 'PTree', which consists of a list of predecessor nodes and the edges that led
--   to them.
type Level n e = [(Node, n, e)]

-- | The 'PTree' of a node in a graph is a list of 'Level's, the length of which encodes the
--   "depth" of the node. A node with no predecessors has an emtpy 'PTree', while a cyclic graph may
--   contain nodes with infinite 'PTree's.
data PTree n e = PTree Node n [Level n e] deriving (Eq, Ord, Show)

-- | A catamorphism that reduces a 'Level' of a 'PTree' to an abstract "record."
type LevelFold n e r = Level n e -> r

-- | A 'PathLink' is either a path-terminating node, or an interior node with its edge to the
--   next node.
data PathLink n e = INode (Node, n, e) | TNode (Node, n) deriving (Eq, Ord, Show)

-- | A 'Path' is a list of path links.
type Path n e = [PathLink n e]

-- | A catamorphism that reduces a 'Path' to an abstract "record."
type PathFold n e r = Path n e -> r

-- | Compute the prefix tree of a node in a graph.
pTree :: Graph gr => gr n e -> Node -> PTree n e
pTree g n = PTree n (fromJust $ lab g n) (f [n] [])
    where f ns ls
            | null $ n' ns = map (map (\(x, y) -> (x, fromJust $ lab g x, y))) ls
            | otherwise    = f (n' ns) (concatMap e ns : ls)
          e  = lpre g
          n' = fastNub . concatMap (pre g)

-- | Compute the prefix trees of all sink nodes in a graph. This is guaranteed not to produce any
-- degenerate 'PTree's, i.e. trees that also exist as possible sub-trees of other trees.
sinkPTrees :: Graph gr => gr n e -> [PTree n e]
sinkPTrees g = map (pTree g) (sinkNodes g)

-- | Compute the depth of a 'PTree'.
treeDepth :: PTree n e -> Int
treeDepth (PTree _ _ ls) = length ls

-- | Compute the shape of a 'PTree', i.e. the number of nodes in each level.
treeShape :: PTree n e -> [Int]
treeShape (PTree _ _ ls) = map length ls

-- | Test whether a node is a "source node," i.e. it has no predecessors.
isSourceNode :: Graph gr => gr n e -> Node -> Bool
isSourceNode g = null . pre g

-- | Given a graph, find all "source nodes," i.e. nodes with no predecessors.
sourceNodes :: Graph gr => gr n e -> [Node]
sourceNodes g = filter (isSourceNode g) (nodes g)

-- | Test whether a node is a "sink node," i.e. it has no successors.
isSinkNode :: Graph gr => gr n e -> Node -> Bool
isSinkNode g = null . suc g

-- | Given a graph, find all "sink nodes," i.e. nodes with no predecessors.
sinkNodes :: Graph gr => gr n e -> [Node]
sinkNodes g = filter (isSinkNode g) (nodes g)
