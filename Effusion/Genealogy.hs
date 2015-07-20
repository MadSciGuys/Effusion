{-|
Module      : Effusion.Genealogy
Description : Framework for Genealogy Computation
Copyright   : Travis Whitaker 2014-2015
License     : All rights reserved.
Maintainer  : twhitak@its.jnj.com
Stability   : Provisional
Portability : POSIX

Document this.
-}

{-# LANGUAGE OverloadedStrings #-}

module Effusion.Genealogy (
    -- * Genealogy Data Model
    Lot(..)
  , Flux(..)
  , GenPath(..)
  , Link(..)
  , toLink
  , toGenGraph
  , forwardUnboundPaths
  , reverseUnboundPaths
  , forwardBoundPaths
  , reverseBoundPaths
    -- * Data Integration with Genealogical Context
  , AdHocTarget(..)
  , AdHocSource(..)
  , toAdHocSource
  , mergeAdHocSources
    -- * Utility Functions
  , dequote
  , renderLotBase
  , renderLotAdHoc
  , renderFluxBase
  , renderFluxAdHoc
  ) where

import qualified Data.ByteString.Lazy.Char8 as B  ( ByteString
                                                  , empty
                                                  , init
                                                  , tail
                                                  , intercalate
                                                  )
import qualified Data.ByteString.Builder    as BU ( Builder
                                                  , charUtf8
                                                  , lazyByteString
                                                  , toLazyByteString
                                                  )

import qualified Data.Map                   as M ( Map
                                                 , fromList
                                                 , findWithDefault
                                                 , (!)
                                                 )

import Data.Graph.Inductive.Graph ( Node
                                  , Graph
                                  , DynGraph
                                  , nmap
                                  , emap
                                  , indeg
                                  , nodes
                                  , lab
                                  , lsuc
                                  )
import Data.Graph.Inductive.Basic (grev)
import Data.Graph.Inductive.NodeMap ( NodeMap
                                    , mkMapGraph
                                    )

import Data.Maybe (fromJust)

import Data.Monoid ((<>), mconcat)

import Control.DeepSeq (NFData(rnf))

import Text.Regex.TDFA ((=~))

import Effusion.Data (fastNub)

-- | As far as this module is concerned, a "lot" is an individually tracked
--   unit of mass in a process, with a uniquely identifying 'lotNum', a
--   'materialName' that uniquely identifies a material's role in a process,
--   and an arbitrary list of additional 'lotAdHoc' records.
data Lot = Lot {
    lotNum        :: B.ByteString
   ,materialName  :: B.ByteString
   ,lotAdHoc      :: [[B.ByteString]]
   } deriving (Eq, Ord, Show)

instance NFData Lot where
    rnf (Lot l m a) = rnf l `seq` rnf m `seq` rnf a

-- | As far as this module is concerned, a "flux" is a transfer of mass from one
--   'Lot' to another, uniquely identified by a 'batch' identifier. This type
--   includes date, quantity, and unit records for the "in" and "out" sides of
--   the transaction; the precise semantics of these fields varies among source
--   systems. 'fluxAdHoc' allows for an arbitrary list of associated records.
data Flux = Flux {
    dateIn    :: B.ByteString
   ,quantIn   :: B.ByteString
   ,unitIn    :: B.ByteString
   ,batch     :: B.ByteString
   ,dateOut   :: B.ByteString
   ,quantOut  :: B.ByteString
   ,unitOut   :: B.ByteString
   ,fluxAdHoc :: [[B.ByteString]]
   } deriving (Eq, Ord, Show)

instance NFData Flux where
    rnf (Flux d q u b d' q' u' a) = rnf d  `seq`
                                    rnf q  `seq`
                                    rnf u  `seq`
                                    rnf b  `seq`
                                    rnf d' `seq`
                                    rnf q' `seq`
                                    rnf u' `seq`
                                    rnf a

-- | Simple algebra used to indicate whether or not a source of ad-hoc data is
--   associated with 'Lot' or 'Flux' objects in the genealogy graph.
data AdHocTarget = ToLot | ToFlux deriving (Eq, Ord, Show)

-- | A source of ad-hoc data for integration into a genealogy graph.
data AdHocSource = AdHocSource {
    target  :: AdHocTarget                       -- ^ Data source target type.
   ,fields  :: Int                               -- ^ Number of fields.
   ,headers :: [B.ByteString]                    -- ^ Field names.
   ,table   :: M.Map B.ByteString [B.ByteString] -- ^ 'M.Map' from keys
                                                 --   identifying 'Lot's or
                                                 --   'Flux's to records.
   } deriving (Eq, Ord, Show)

-- | A possible traversal through the genealogy graph, consisting of a list of
--   triples of the 'Node' identifier, node payload type "a", and edge payload
--   type "b". These "links" appear in an order that may not be immediately
--   obvious. For example, we have a value of type 'GenPath Int Char':
--
--   @
--   (1) -'a'-> (2) -'b'-> (3)
--   @
--
--   This is represented by this type as:
--
--   @
--   [(_, 1, _), (_, 2, 'a'), (_, 3, 'b')]
--   @
--
--   There's probably a smarter way to represent this, but abusing tuples and
--   'undefined' in this way makes pattern deconstruction for rendering into a
--   textual data block very clear.
type GenPath a b = [(Node, a, b)]

-- | A single 'Lot' -> 'Flux' -> 'Lot' relationship; a list of these is used to
--   construct the genealogy graph.
type Link = (Lot, Lot, Flux)

toAdHocSource :: AdHocTarget      -- ^ Data source target type.
              -> [[B.ByteString]] -- ^ List of records, including the header
                                  --   names as the first record.
              -> Int              -- ^ Index (from zero) of the record field
                                  -- ^ containing the "key" to a 'Lot' or
                                  --   'Flux'.
              -> AdHocSource
toAdHocSource t (h:cs) i = AdHocSource t lh h m
    where lh = length h
          m  = M.fromList $ map (\c -> (c !! i, c)) cs

-- | Insert all of the data in each 'AdHocSource' provided into the respective
--   'Lot's or 'Flux's in a genealogy graph.
mergeAdHocSources :: DynGraph gr => gr Lot Flux -> [AdHocSource] -> gr Lot Flux
mergeAdHocSources = foldr f
    where f (AdHocSource ToLot  ar h m) g = nmap (mergeLot  m ar) g
          f (AdHocSource ToFlux ar h m) g = emap (mergeFlux m ar) g
          mergeLot  m i l = let r = M.findWithDefault (empty i) (lotNum l) m
                            in l { lotAdHoc = r:(lotAdHoc l)}
          mergeFlux m i f = let r = M.findWithDefault (empty i) (batch f) m
                            in f { fluxAdHoc = r:(fluxAdHoc f) }
          empty = flip replicate B.empty

toLink :: [B.ByteString] -> Link
toLink [ lin
       , min
       , din
       , qin
       , uin
       , bn
       , lout
       , mout
       , dout
       , qout
       , uout
       ] = ((Lot lin min []), (Lot lout mout []), (Flux din qin uin bn dout qout uout []))
toLink _ = error "toLink: Pattern match failure."

-- | Build a genealogy graph from a list of records specifying individual
--   genealogy 'Link's, along with a 'NodeMap'.
toGenGraph :: DynGraph gr => [[B.ByteString]] -> (gr Lot Flux, NodeMap Lot)
toGenGraph lks = mkMapGraph ls fs
    where fs = map toLink lks
          ls = fastNub $ concatMap (\(a, b, _) -> [a, b]) fs

-- | Compute all possible 'GenPath's from all nodes in the provided genealogy
--   graph without predecessors.
forwardUnboundPaths :: Graph gr => gr a b -> [GenPath a b]
forwardUnboundPaths g = paths
    where sources = filter ((0 ==) . (indeg g)) $ nodes g
          nextPaths l@((n, _, _):ps) = let ss = map (\(n', e) -> (n', fromJust $ lab g n', e)) (lsuc g n)
                                       in map (:l) ss
          walk ps = let ps' = concatMap nextPaths ps
                    in case ps' of [] -> map reverse ps
                                   _  -> walk ps'
          paths = concatMap (\n -> walk [[(n, undefined, undefined)]]) sources

-- | Compute all possible 'GenPath's from all nodes in the provided genealogy
--   graph without successors.
reverseUnboundPaths :: DynGraph gr => gr a b -> [GenPath a b]
reverseUnboundPaths = forwardUnboundPaths . grev

-- | Render a path as a string for easy debugging or validation.
prettyPrintPath [] = "[]"
prettyPrintPath (x:xs) = ((show . (\(a, b, _) -> (a, b))) x) ++ ":" ++ (show xs)

-- | Compute all possible 'GenPath's from all nodes in the provided genealogy
--   graph without predecessors, where the highest ranking node does not have
--   a rank greater than the provided limit. The origin node has a rank of
--   zero; for example, all paths of /length/ 5 have a maximum node /rank/ of 4.
forwardBoundPaths :: (Graph gr, Ord a, Ord b) => gr a b -> Int -> [GenPath a b]
forwardBoundPaths g lim = paths
    where sources = (filter ((0 ==) . (indeg g)) $ nodes g)
          nextPaths l@((n, _, _):ps) = let ss = map (\(n', e) -> (n', fromJust $ lab g n', e)) (lsm M.! n)
                                       in map (:l) ss
          lsm = M.fromList $ map (\x -> (x, fastNub $ lsuc g x)) (nodes g)
          walk 0 ps = map reverse ps
          walk i ps = let ps' = concatMap nextPaths ps
                      in case ps' of [] -> map reverse ps
                                     _  -> walk (i-1) ps'
          paths = concatMap (\n -> walk lim [[(n, fromJust $ lab g n, undefined)]]) $ fastNub sources

-- | Compute all possible 'GenPath's from all nodes in the provided genealogy
--   graph without successors, where the highest ranking node does not have
--   a rank greater than the provided limit. The origin node has a rank of
--   zero; for example, all paths of /length/ 5 have a maximum node /rank/ of 4.
reverseBoundPaths :: (DynGraph gr, Ord a, Ord b) => gr a b -> Int -> [GenPath a b]
reverseBoundPaths = forwardBoundPaths . grev

-- | Clean up 4store's bytestream. This function probably doesn't belong in
--   this module.
dequote :: [[B.ByteString]] -> [[B.ByteString]]
dequote = map $ map (B.init . B.tail . unblank)
    where unblank x = if (x =~ (":b[[:xdigit:]]+" :: B.ByteString))
                      then "\"\""
                      else x

renderLotBase :: Lot -> BU.Builder
renderLotBase l = BU.lazyByteString (lotNum l) <>
                  BU.charUtf8 ','              <>
                  BU.lazyByteString (materialName l)

renderLotAdHoc :: Lot -> Int -> BU.Builder
renderLotAdHoc l i = let (c:cs) = (lotAdHoc l) !! i
                     in BU.lazyByteString c <> mconcat [BU.charUtf8 ',' <> BU.lazyByteString c' | c' <- cs]

renderFluxBase :: Flux -> BU.Builder
renderFluxBase f = BU.lazyByteString (dateIn f)   <> c <>
                   BU.lazyByteString (quantIn f)  <> c <>
                   BU.lazyByteString (unitIn f)   <> c <>
                   BU.lazyByteString (batch f)    <> c <>
                   BU.lazyByteString (dateOut f)  <> c <>
                   BU.lazyByteString (quantOut f) <> c <>
                   BU.lazyByteString (unitOut f)
                        where c = BU.charUtf8 ','

renderFluxAdHoc :: Flux -> Int -> BU.Builder
renderFluxAdHoc f i = let (c:cs) = (fluxAdHoc f) !! i
                      in BU.lazyByteString c <> mconcat [BU.charUtf8 ',' <> BU.lazyByteString c' | c' <- cs]
