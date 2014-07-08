{-|
Module      : Effusion.Genealogy
Description : Write this.
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
    Level

    -- * Tree-based Models
   ,pTree
) where

import Data.List                  (concatMap)
import Data.Maybe                 (fromJust)
import Data.Graph.Inductive.Graph (Node, Graph, pre, lpre, lab)

import Effusion.Data              (fastNub)

type Level n e = [(Node, n, e)]

pTree :: Graph gr => gr a b -> Node -> (Node, [Level a b])
pTree g n = (n, f [n] [])
    where f ns ls
            | null $ n' ns = map (map (\(x, y) -> (x, fromJust $ lab g x, y))) ls
            | otherwise    = f (n' ns) (concatMap e ns : ls)
          e  = lpre g
          n' = fastNub . concatMap (pre g)
