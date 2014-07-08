{-|
Module      : Effusion.Data
Description : Framework for Information Structure Transformation
Copyright   : Travis Whitaker 2014
License     : All rights reserved.
Maintainer  : twhitak@its.jnj.com
Stability   : Provisional
Portability : POSIX

Effusion.Data provides utilities for transformation between various formats and information
structures.
-}

{-# LANGUAGE OverloadedStrings #-}

module Effusion.Data (

    -- * Tables
    markArity

    -- ** Generic Functions
   ,markArityGeneric

    -- * Lists
   ,deleteN
   ,fastNub
   ,fastNubPairs

    -- ** Generic Functions
   ,deleteNGeneric
) where

import Data.List (genericSplitAt)

import Data.ByteString.Lazy.Char8 as C (ByteString, pack, intercalate, append)
import Data.Set                   as S (toList, fromList, empty, insert)

-- | Given a list of `C.ByteString`s representing the field names of a table header, mark each
--   header with the given arity and concatenate the result.
markArity :: [C.ByteString] -> Int -> C.ByteString
markArity xs n = C.intercalate "," xs'
    where xs' = map (C.pack (show n) `C.append` ":" `C.append`) xs

-- | Generic implementation of `markArity`.
markArityGeneric :: (Integral a, Show a) => [C.ByteString] -> a -> C.ByteString
markArityGeneric xs n = C.intercalate "," xs'
    where xs' = map (C.pack (show n) `C.append` ":" `C.append`) xs

-- | Delete the /n/th element from a list, assuming the list is 1-indexed. This is a special case of
-- `deleteGeneric`.
deleteN :: Int -> [a] -> [a]
deleteN i xs = ys ++ tail zs
    where (ys, zs) = splitAt (i - 1) xs

-- | Like `nub` from the `Prelude` but much faster and with the additional constraint that elements
-- are orderable. Order is not preserved.
fastNub :: (Eq a, Ord a) => [a] -> [a]
fastNub = S.toList . S.fromList

-- | Like `fastNub`, but operable on a list of pairs.
fastNubPairs :: (Eq a, Ord a) => [(a, a)] -> [a]
fastNubPairs ps = S.toList $ foldl f S.empty ps
    where f s (a, b) = S.insert b $ S.insert a s

-- | Generic implementation of `delete`.
deleteNGeneric :: Integral a => a -> [b] -> [b]
deleteNGeneric i xs = ys ++ tail zs
    where (ys, zs) = genericSplitAt (i - 1) xs
