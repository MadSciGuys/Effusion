module Effusion.Inference (
    -- * List/String Analytics
    levenshtein
   ,relativeLevenshtein
   ,lexPerms
   ,lexPairs
) where

import Data.Array (range, listArray, (!))
import Data.List  (genericLength, genericTake, permutations, subsequences)

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
levenshtein :: Eq a => [a] -> [a] -> Integer
levenshtein a b = d m n
    where m = genericLength a
          n = genericLength b

          -- Transform input to 1-indexed array:
          a' = listArray (1,m) a
          b' = listArray (1,n) b

          bounds = ((0, 0), (m, n))
          indices = range bounds
          ds = listArray bounds [d i j | (i, j) <- indices]

          d i 0 = i
          d 0 j = j
          d i j
            | a' ! i == b' ! j = ds ! (i-1, j-1) -- Element equality
            | otherwise = minimum [ ds ! (i-1, j)   + 1   -- Deletion
                                   ,ds ! (i, j-1)   + 1   -- Insertion
                                   ,ds ! (i-1, j-1) + 1 ] -- Substitution

-- | Compute the relative Levenshtein distance between two lists. This is 'levenshtein' divided by
-- the length of the longer of the two input lists. This allows for the comparison of Levenshtein
-- distances among pairs of lists.
relativeLevenshtein :: Eq a => [a] -> [a] -> Double
relativeLevenshtein a b = lev / maxlen
    where lev    = fromIntegral $ levenshtein a b
          maxlen = fromIntegral $ maximum [integerLength a, integerLength b]
          integerLength :: [a] -> Integer
          integerLength = genericLength

-- | Compute @n@ lexicographical permutations of list of elements.
lexPerms :: Integral a => a -> [b] -> [[b]]
lexPerms n xs = genericTake n xs'
    where xs' = concatMap permutations (subsequences $ cycle xs)

-- | Provide @n@ pairs of lexicographical string permutations. Useful for testing fuzzy string
-- matches. Note that the length of the returned string ins @n^2@.
lexPairs :: Integral a => a -> [b] -> [([b], [b])]
lexPairs n xs = [(a, b) | a <- lexPerms n xs, b <- lexPerms n xs]
