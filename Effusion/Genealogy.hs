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

module Effusion.Genealogy (
    GenealogyNode(..)
) where

import Data.Tree (Tree)

class GenealogyNode n where
    type Predecessor n :: *
    type Successor n   :: *
    predecessors       :: n -> [Predecessor n]
    successors         :: n -> [Successor n]
