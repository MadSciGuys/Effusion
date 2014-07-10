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
   ,markString

    -- ** Generic Functions
   ,markArityGeneric

    -- * Lists
   ,deleteN
   ,fastNub
   ,fastNubPairs
   ,uniquePairs
   ,ngram
   ,ngrams
   ,freqTable
   ,manhattanList
   ,insertAdj
   ,lexPermutations
   ,lexPairs

    -- ** Generic Functions
   ,deleteNGeneric
   ,ngramGeneric
   ,ngramsGeneric
   ,lexPermutationsGeneric
   ,lexPairsGeneric
) where

import Data.List (tails, permutations, subsequences, genericSplitAt, genericLength, genericTake)

import Data.ByteString.Lazy.Char8 as C  (ByteString, pack, intercalate, append)
import Data.Set                   as S  (toList, fromList, empty, insert)
import Data.MultiSet              as MS (fromList, toOccurList)

-- | Given a list of 'C.ByteString's representing the field names of a table header, mark each
--   header with the given arity and concatenate the result.
markArity :: Int -> [C.ByteString] -> C.ByteString
markArity n xs = C.intercalate "," xs'
    where xs' = map (C.pack (show n) `C.append` ":" `C.append`) xs

-- | Given a list of `C.ByteString's representing the field names of a table header, mark each
--   header with the given string /without/ concatenating the result.
markString :: C.ByteString -> [C.ByteString] -> [C.ByteString]
markString s = map (s `C.append` ":" `C.append`)

-- | Generic implementation of 'markArity'.
markArityGeneric :: (Integral a, Show a) => [C.ByteString] -> a -> C.ByteString
markArityGeneric xs n = C.intercalate "," xs'
    where xs' = map (C.pack (show n) `C.append` ":" `C.append`) xs

-- | Delete the /n/th element from a list, assuming the list is 1-indexed. This is a special case of
--   'deleteGeneric'.
deleteN :: Int -> [a] -> [a]
deleteN i xs = ys ++ tail zs
    where (ys, zs) = splitAt (i - 1) xs

-- | Like 'nub' from the 'Prelude' but much faster and with the additional constraint that elements
--   are orderable. Order is not preserved.
fastNub :: (Eq a, Ord a) => [a] -> [a]
fastNub = S.toList . S.fromList

-- | Like 'fastNub', but operable on a list of pairs.
fastNubPairs :: (Eq a, Ord a) => [(a, a)] -> [a]
fastNubPairs ps = S.toList $ foldl f S.empty ps
    where f s (a, b) = S.insert b $ S.insert a s

-- | Compute the number of unique pairings in a list of elements, ignoring the order of the
--   elements in the pair, i.e.
uniquePairs :: Ord a => [a] -> [(a, a)]
uniquePairs xs = [(x, y) | (x : ys) <- tails $ fastNub xs, y <- ys]

-- | Compute the /n/th n-gram of a list.
ngram :: Int -> [a] -> [[a]]
ngram n xs
    | n <= length xs = take n xs : ngram n (drop 1 xs)
    | otherwise = []

-- | Compute all possible n-grams of a list.
ngrams :: [a] -> [[[a]]]
ngrams xs = map (flip ngram xs) [1..length xs]

-- | Compute the frequency table of a list of orderable elements.
freqTable :: Ord a => [a] -> [(a, Int)]
freqTable = MS.toOccurList . MS.fromList

-- | Arity-generic Manhattan distance implementation using lists. The result is only sensible if the
--   lists are of the same length; this precondition is not checked.
manhattanList :: Num a => [a] -> [a] -> a
manhattanList = ((sum . map abs) .) . zipWith (-)

-- | Given a pivot element, an element for insertion, and a list of equatable elements, insert the
--   new element adjacent to the pivot whereever the pivot occurs in the list. IF the pivot occurs
--   more than once in the input list, the returned list will ahve multiple copies of the inserted
--   element. One might think of this as a selective version of 'intersperse'.
insertAdj :: Eq a =>  a  -- ^ Pivot element
                  ->  a  -- ^ New element
                  -> [a] -- ^ Old list
                  -> [a] -- ^ New list
insertAdj _ _ [] = []
insertAdj p i xs = reverse $ foldl f [] xs
    where f ds x
            | x == p    = i : p : ds
            | otherwise = x : ds

-- | Compute /n/ lexicographical permutations of list of elements. 'cycle' is used if the input
--   list is too short.--
lexPermutations :: Int -> [a] -> [[a]]
lexPermutations n xs = take n xs'
    where xs' = concatMap permutations (subsequences $ cycle xs)

-- | Provide /n/ pairs of lexicographical string permutations. Useful for testing fuzzy string
--   matches. Note that the length of the returned string is /n^2/.
lexPairs :: Int -> [a] -> [([a], [a])]
lexPairs n xs = [(a, b) | a <- lexPermutations n xs, b <- lexPermutations n xs]

-- | Generic implementation of 'delete'.
deleteNGeneric :: Integral a => a -> [b] -> [b]
deleteNGeneric i xs = ys ++ tail zs
    where (ys, zs) = genericSplitAt (i - 1) xs

-- | Generic implementation of 'ngram'.
ngramGeneric :: Integral a => a -> [b] -> [[b]]
ngramGeneric n xs
    | n <= genericLength xs = genericTake n xs : ngramGeneric n (drop 1 xs)
    | otherwise = []

-- | Generic implementation of 'ngrams'. This is slower, but won't fail on integer overflow due to
--   huge lists.
ngramsGeneric :: [a] -> [[[a]]]
ngramsGeneric xs = map (flip ngramGeneric xs) [1..genericLength xs]

-- | Generic implementation of 'lexPermutations'.
lexPermutationsGeneric :: Integral a => a -> [b] -> [[b]]
lexPermutationsGeneric n xs = genericTake n xs'
    where xs' = concatMap permutations (subsequences $ cycle xs)

-- | Provide /n/ pairs of lexicographical string permutations. Useful for testing fuzzy string
--   matches. Note that the length of the returned string is /n^2/.
lexPairsGeneric :: Integral a => a -> [b] -> [([b], [b])]
lexPairsGeneric n xs = [(a, b) | a <- lexPermutationsGeneric n xs, b <- lexPermutationsGeneric n xs]
