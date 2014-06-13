{-|
Module      : Effusion.Inverence
Description : Framework for Describing and Executing Merges on Data
Copyright   : Travis Whitaker 2014
License     : All rights reserved.
Maintainer  : twhitak@its.jnj.com
Stability   : Provisional
Portability : POSIX

Effusion.Inference provides utilities for /merging/ or /mapping/ between data. This is often
required in heterogeneous environments. The goal of this module is to simplify and generalize what
is often the most repetitive, tedious, and fragile component of heterogeneous data management
systems.
-}
module Effusion.Inference (

    -- * List/String Analytics

    levenshtein
   ,jaccard
   ,normalLevenshtein
   ,fuzzyMatch
   ,fuzzyMatchT
   ,fuzzyRank
   ,fuzzyGroup

    -- ** ByteString Functions

   ,levenshteinBS
   ,jaccardBS
   ,normalLevenshteinBS
   ,fuzzyMatchBS
   ,fuzzyMatchTBS
   ,fuzzyRankBS
   ,fuzzyGroupBS

   -- * Utility Functions

   ,insertAdj
   ,lexPermutations
   ,lexPairs
) where

import Data.List (length, genericTake, intersect, union, sortBy, permutations, subsequences)
import qualified Data.Array            as A (range, listArray, (!))
import qualified Data.Map              as M ((!), fromList)
import qualified Data.ByteString.Char8 as C (ByteString, length, index, null, unpack)

-- | Compute the Levenshtein distance between two lists. Informally, the Levenshtein distance
-- between two lists of equatable elements is the minimum number of sigle-element changes
-- (insertions, deletions, or substitutions) required to transform one list into the other. The
-- following naive implementation is illustrative:
--
-- > naive a b = d m n
-- >     where d i 0 = i
-- >           d 0 j = j
-- >           d i j
-- >             | a !! (i-1) == b !! (j-1) = d (i-1) (j-1)
-- >             | otherwise = minimum [ d (i-1) j     + 1 --(delete)
-- >                                    ,d i (j-1)     + 1 --(insert)
-- >                                    ,d (i-1) (j-1) + 1 --(substitute)
-- >                                   ]
--
-- However, this naive implementation carries an exponential time complexity. 'levenshtein' instead
-- uses a lazy immutable 'Array' to cache calls to @d@, allowing for /O(mn)/ time complexity.
levenshtein :: Eq a => [a] -> [a] -> Int
levenshtein a b = d m n
    where m = length a
          n = length b

          -- Transform input to 1-indexed array:
          a' = A.listArray (1,m) a
          b' = A.listArray (1,n) b

          bounds = ((0, 0), (m, n))
          indices = A.range bounds
          ds = A.listArray bounds [d i j | (i, j) <- indices]

          d i 0 = i
          d 0 j = j
          d i j
            | a' A.! i == b' A.! j = ds A.! (i-1, j-1)      -- Element equality
            | otherwise = minimum [ ds A.! (i-1, j)   + 1   -- Deletion
                                   ,ds A.! (i, j-1)   + 1   -- Insertion
                                   ,ds A.! (i-1, j-1) + 1 ] -- Substitution

-- | Compute the Jaccard distance between two lists. The Jaccard Index of a pair of sets is
-- defined as the size of the intersection of the sets divided by the size of the union of the
-- sets. This implies:
--
-- prop> 0 <= jaccard a b <= 1
--
-- This function returns the Jaccard distance, which is simply one minus the Jaccard Index. This
-- provides a metric of list difference rather than similarity. The Jaccard distance is generally
-- "less strict" than the 'levenshtein' distance, i.e. it returns lower values for plausibly
-- related strings. Unlike 'levenshtein' this is a set-theoretic metric, so 'jaccard' should be
-- used when the order of the lists and/ore presence of repeated elements isn't relevant.
jaccard :: Eq a => [a] -> [a] -> Double
jaccard a b = 1 - (intersectionLength / unionLength)
    where intersectionLength = fromIntegral $ length $ intersect a b
          unionLength        = fromIntegral $ length $ union a b

-- | Compute the normalized Levenshtein distance between two lists. This is 'levenshtein' divided
-- by the length of the longer of the two input lists. This allows for the meaningful comparison of
-- Levenshtein distances among pairs of lists.
--
-- The normalized Levenshtein distance is generally "more strict" than the 'jaccard' distance, i.e.
-- it returns greater values for plausibly related strings.
normalLevenshtein :: Eq a => [a] -> [a] -> Double
normalLevenshtein a b = lev / maxlen
    where lev    = fromIntegral $ levenshtein a b
          maxlen = fromIntegral $ maximum [length a, length b]

-- | Given a scoring function, a reference list, and a list of candidate matching lists, return the
-- best matched list(s). It is assumed that the scoring function will return a 'Double' between one
-- and zero (like the 'normalLevenshtein' or 'jaccard' functions) and that a lower score indicates
-- more closely related lists. If all of the candidate lists earn a score of one, the empty list is
-- returned. If two or more candidate lists tie, they will be returned together. Any duplicates in
-- the candidate list will always tie, in this case 'fuzzyMatch' will return two (or more) copies
-- if the copies are the best match. A few special cases:
--
-- > fuzzyMatch _ _  []     = []
-- > fuzzyMatch _ [] cs     = filter null cs
-- > fuzzyMatch s r  (c:[]) = filter (\x -> s r x /= 1.0) [c]
fuzzyMatch :: Eq a => ([a] -> [a] -> Double) -- ^ Scoring function
                   ->  [a]                   -- ^ Reference
                   -> [[a]]                  -- ^ Candidates
                   -> [[a]]                  -- ^ Best-matched candidate(s)
fuzzyMatch _ _  []     = []
fuzzyMatch _ [] cs     = filter null cs
fuzzyMatch s r  (c:[]) = filter (\x -> s r x /= 1.0) [c]
fuzzyMatch s r  (c:cs) = filter (\x -> s r x /= 1.0) cs'
    where cs' = snd $ foldl compare (s r c, [c]) cs
          compare quo@(h, m:ms) x
            | s' > h  = quo
            | s' == h = (h, x:m:ms)
            | s' < h  = (s', [x])
                where s' = s r x

-- | Given a scoring function, a score threshold, a reference list, and a list of candidate matching
-- lists, return the best matched list(s) whose score is under the threshold. It is assumed that the
-- scoring function will return a 'Double' between one and zero (like the 'normalLevenshtein' or
-- 'jaccard' functions) and that a lower score indicates more closely related lists. Likewise, the
-- threshold must be a 'Double' between one and zero. If all of the candidate lists earn a score of
-- one, the empty list is returned. If two or more candidate lists tie, they will be returned
-- together. Any duplicates in the candidate list will always tie, in this case 'fuzzyMatchT' will
-- return two (or more) copies if the copies are the best match.
fuzzyMatchT :: Eq a => ([a] -> [a] -> Double) -- ^ Scoring function
                    -> Double                 -- ^ Score threshold
                    ->  [a]                   -- ^ Reference
                    -> [[a]]                  -- ^ Candidates
                    -> [[a]]                  -- ^ Best-matched candidate(s)
fuzzyMatchT _ _ _  []     = []
fuzzyMatchT _ _ [] cs     = filter null cs
fuzzyMatchT s t r  (c:[]) = filter (\x -> (s r x /= 1.0) && (s r x <= t)) [c]
fuzzyMatchT s t r  (c:cs) = filter (\x -> (s r x /= 1.0) && (s r x <= t)) cs'
    where cs' = snd $ foldl compare (s r c, [c]) cs
          compare quo@(h, m:ms) x
            | s' > h  = quo
            | s' == h = (h, x:m:ms)
            | s' < h  = (s', [x])
                where s' = s r x

-- | Given a scoring function, a reference list, and a list of candidate matching lists, return the
-- list of candidates sorted in order of best-matching to worst-matching. It is assumed that the
-- scoring function will return a 'Double' between one and zero (like the 'normalLevenshtein' and
-- 'jaccard' functions) and that a lower score indicates more closely related lists.
fuzzyRank :: (Eq a, Ord a) => ([a] -> [a] -> Double) -- ^ Scoring function
                           ->  [a]                   -- ^ Reference
                           -> [[a]]                  -- ^ Candidates
                           -> [[a]]                  -- ^ Sorted candidates
fuzzyRank s r cs = sortBy compare cs
    where compare x y
            | s' M.! x >  s' M.! y  = GT
            | s' M.! x == s' M.! y  = EQ
            | s' M.! x <  s' M.! y  = LT
          s' = M.fromList [(k, s r k) | k <- cs]

-- | Document this.
fuzzyGroup :: (Eq a, Ord a) => ([a] -> [a] -> Double)
                            -> [[a]]
                            -> [[a]]
fuzzyGroup _ l@[]        = l
fuzzyGroup s l@(x:[])    = l
fuzzyGroup s l@(x:x':[]) = l
fuzzyGroup s l@(x:x':xs) = foldl f [x,x'] xs
    where f ys i = g [] ys (fuzzyRank s i ys)
              where g gs (z:z':[]) _  = reverse gs ++ (if (m M.! (z,i)) <= (m M.! (z,z')) then [z,i,z']
                                                                                          else [z,z',i])
                    g gs (z:z':zs) [] = reverse gs ++ ((z:z':zs) ++ [i])
                    g gs (z:z':zs) (p:ps)
                      | z == p    = if (m M.! (z,i)) <= (m M.! (z,z')) then reverse gs ++ (z:i:z':zs)
                                                                       else g [] (reverse gs ++ (z:z':zs)) ps
                      | otherwise = g (z:gs) (z':zs) (p:ps)
          m = M.fromList [((a,b), s a b) | a <- l, b <- l]

-- | Compute the Levenshtein distance between two 'C.ByteString's, like 'levenshtein'.
levenshteinBS :: C.ByteString -> C.ByteString -> Int
levenshteinBS a b = d m n
    where m = C.length a
          n = C.length b

          bounds = ((0, 0), (m, n))
          indices = A.range bounds
          ds = A.listArray bounds [d i j | (i, j) <- indices]

          d i 0 = i
          d 0 j = j
          d i j
            | C.index a (i-1) == C.index b (j-1) = ds A.! (i-1, j-1) -- Element equality
            | otherwise = minimum [ ds A.! (i-1, j)   + 1    -- Deletion
                                   ,ds A.! (i, j-1)   + 1    -- Insertion
                                   ,ds A.! (i-1, j-1) + 1 ]  -- Substitution

-- | Compute the Jaccard distance between two 'C.ByteString's, like 'jaccard'. Unlike 'List's,
-- 'C.ByteString's don't support efficient 'intersect' and 'union' operations, so this function is
-- very slow.
jaccardBS :: C.ByteString -> C.ByteString -> Double
jaccardBS a b = 1 - (intersectionLength / unionLength)
    where intersectionLength = fromIntegral $ length $ intersect a' b'
          unionLength        = fromIntegral $ length $ union a' b'
          a' = C.unpack a
          b' = C.unpack b

-- | Compute the normalized Levenshtein distance between two 'C.ByteString's, like
-- 'normalLevenshtein'.
normalLevenshteinBS :: C.ByteString -> C.ByteString -> Double
normalLevenshteinBS a b = lev / maxlen
    where lev    = fromIntegral $ levenshteinBS a b
          maxlen = fromIntegral $ maximum [C.length a, C.length b]

-- | Given a scoring function, a reference 'C.ByteString', and a list of candidate matching
-- 'C.ByteString's, return the best matched 'C.ByteString'(s), like 'fuzzyMatch'.
fuzzyMatchBS :: (C.ByteString -> C.ByteString -> Double) -- ^ Scoring function
             ->  C.ByteString                            -- ^ Reference
             -> [C.ByteString]                           -- ^ Candidates
             -> [C.ByteString]                           -- ^ Best-matched candidate(s)
fuzzyMatchBS _ _  [] = []
fuzzyMatchBS s r l@(c:cs)
    | C.null r  = filter C.null l
    | otherwise = filter (\x -> s r x /= 1.0) cs'
    where cs' = snd $ foldl compare (s r c, [c]) cs
          compare quo@(h, m:ms) x
            | s' > h = quo
            | s' == h = (h, x:m:ms)
            | s' < h = (s', [x])
                where s' = s r x

-- | Given a scoring function, a score threshold, a reference 'C.ByteString', and a list of
-- candidate matching 'C.ByteString's, return the best matched 'C.ByteString'(s) whose score is
-- less than or equal to the threshold, like 'fuzzyMatchT'.
fuzzyMatchTBS :: (C.ByteString -> C.ByteString -> Double) -- ^ Scoring function
             ->    Double                                 -- ^ Score threshold
             ->  C.ByteString                             -- ^ Reference
             -> [C.ByteString]                            -- ^ Candidates
             -> [C.ByteString]                            -- ^ Best-matched candidate(s)
fuzzyMatchTBS _ _ _  [] = []
fuzzyMatchTBS s t r l@(c:cs)
    | C.null r  = filter C.null l
    | otherwise = filter (\x -> (s r x /= 1.0) && (s r x <= t)) cs'
    where cs' = snd $ foldl compare (s r c, [c]) cs
          compare quo@(h, m:ms) x
            | s' > h = quo
            | s' == h = (h, x:m:ms)
            | s' < h = (s', [x])
                where s' = s r x

-- | Given a scoring function, a reference 'C.ByteString', and a list of candidate matching
-- 'C.ByteString's, return the list of candidates sorted in order of best-matching to
-- worst-matching, like 'fuzzyRank'.
fuzzyRankBS :: (C.ByteString -> C.ByteString -> Double)  -- ^ Scoring function
            ->  C.ByteString                             -- ^ Reference
            -> [C.ByteString]                            -- ^ Candidates
            -> [C.ByteString]                            -- ^ Sorted candidates
fuzzyRankBS s r cs = sortBy compare cs
    where compare x y
            | s' M.! x >  s' M.! y  = GT
            | s' M.! x == s' M.! y  = EQ
            | s' M.! x <  s' M.! y  = LT
          s' = M.fromList [(k, s r k) | k <- cs]

-- | Document this.
fuzzyGroupBS :: (C.ByteString -> C.ByteString -> Double)
             -> [C.ByteString]
             -> [C.ByteString]
fuzzyGroupBS _ l@[]        = l
fuzzyGroupBS s l@(x:[])    = l
fuzzyGroupBS s l@(x:x':[]) = l
fuzzyGroupBS s l@(x:x':xs) = foldl f [x,x'] xs
    where f ys i = g [] ys (fuzzyRankBS s i ys)
              where g gs (z:z':[]) _  = reverse gs ++ (if (m M.! (z,i)) <= (m M.! (z,z')) then [z,i,z']
                                                                                          else [z,z',i])
                    g gs (z:z':zs) [] = reverse gs ++ ((z:z':zs) ++ [i])
                    g gs (z:z':zs) (p:ps)
                      | z == p    = if (m M.! (z,i)) <= (m M.! (z,z')) then reverse gs ++ (z:i:z':zs)
                                                                       else g [] (reverse gs ++ (z:z':zs)) ps
                      | otherwise = g (z:gs) (z':zs) (p:ps)
          m = M.fromList [((a,b), s a b) | a <- l, b <- l]

-- | Given a pivot element, an element for insertion, and a list of equatable elements, insert the
-- new element adjacent to the pivot wherever the pivot occurs in the list. If the pivot occurs more
-- than once in the input list, the returned list will have multiple copies of the inserted element.
-- One might think of this as a selective version of 'intersperse'.
insertAdj :: Eq a =>  a  -- ^ Pivot element
                  ->  a  -- ^ New element
                  -> [a] -- ^ Old List
                  -> [a] -- ^ New List
insertAdj _ _ [] = []
insertAdj p i xs = reverse $ foldl f [] xs
    where f ds x
            | x == p    = i:(p:ds)
            | otherwise = x:ds

-- | Compute @n@ lexicographical permutations of list of elements. 'cycle' is used if the input
-- list is too short.
lexPermutations :: Integral a => a -> [b] -> [[b]]
lexPermutations n xs = genericTake n xs'
    where xs' = concatMap permutations (subsequences $ cycle xs)

-- | Provide @n@ pairs of lexicographical string permutations. Useful for testing fuzzy string
-- matches. Note that the length of the returned string is @n^2@.
lexPairs :: Integral a => a -> [b] -> [([b], [b])]
lexPairs n xs = [(a, b) | a <- lexPermutations n xs, b <- lexPermutations n xs]
