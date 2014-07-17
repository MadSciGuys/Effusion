{-|
Module       : Effusion.Numerics
Description  : Functions for Processing and Analysis of Numerical Data.
Copyright    : Travis Whitaker 2014
License      : All rights reserved.
Maintainer   : twhitak@its.jnj.com
Stability    : Provisional
Portability  : POSIX

Document this.
-}
module Effusion.Numerics (

    -- * Types
    Discretizeable(..)

    -- * Means

    -- ** Pythagorean Means
   ,arithmeticMean
   ,geometricMean
   ,harmonicMean

    -- ** Generalized Means

   ,powerMean
   ,cesaro

    -- * Discrete Distribution

    -- * Signal Processing

   ,sample
   ,samplePairs
   ,ewma
   ,ewma'

    -- * General Statistics

   ,median
   ,mode
   ,genericMode
) where

import Control.Monad (liftM2)

import Data.List (foldl', inits, sort, sortBy)
import qualified Data.Map as M (empty, insertWith, toList)

-- | An arbitrarily discretizeable function.
type Discretizeable t a = t -> a

-- | Sum a list in constant memory.
csum :: Num a => [a] -> a
csum = foldl' (+) 0

-- | Product of a list in constant memory.
cproduct :: Num a => [a] -> a
cproduct = foldl' (*) 1

-- | Length of a list as a double, suitable as an argument to '(/)', '(**)', etc.
numericLength :: [a] -> Double
numericLength = fromIntegral . length

-- | Compute the arithmetic mean of a list. The list must be finite; this function is strict in
-- its argument. Very long lists may cause double precision overflow; see 'ewma''.
arithmeticMean :: [Double] -> Double
arithmeticMean = liftM2 (/) sum numericLength

-- | Compute the geometric mean of a list. The list must be finite; this function is strict in its
-- argument. Very long lists may cause double precision overflow; see 'ewma''.
geometricMean :: [Double] -> Double
geometricMean = liftM2 (**) cproduct ((1 /) . numericLength)

-- | Compute the harmonic mean of a list. The list must be finite; this function is strict in its
-- argument. This function tends to work well even on huge lists.
harmonicMean :: [Double] -> Double
harmonicMean xs = numericLength xs / csum (map (1 /) xs)

-- | Compute the power mean (also known as generalized mean) of a list. This function may fail as
-- the characterizing parameter approaches 0. The list must be finite; this function is strict in
-- its second argument. Some notable properties:
--
-- > powerMean -1 = harmonicMean
-- > powerMean  0 = geometricMean
-- > powerMean  1 = arithmeticMean
-- > powerMea   2 = RMS
powerMean :: Double -> [Double] -> Double
powerMean m xs = ((1 / numericLength xs) * csum (map  (** m) xs)) ** (1 / m)

-- | Given a finite sequence, compute the Cesaro sequence.
cesaro :: [Double] -> [Double]
cesaro = map arithmeticMean . tail . inits

-- | Sample a discretizeable function over the provided domain and resolution.
sample :: (Enum t, Num t) => t                  -- ^ Initial value
                 -> t                           -- ^ Final value
                 -> t                           -- ^ Increment
                 -> Discretizeable t a          -- ^ Sampling function
                 -> [a]                         -- ^ List of function samples
sample x x' i f = map f [x, (x + i) .. x']

-- | Sample a discretizeable function over the provided domain and resolution.
samplePairs :: (Enum t, Num t) => t                  -- ^ Initial value
                      -> t                           -- ^ Final value
                      -> t                           -- ^ Increment
                      -> Discretizeable t a          -- ^ Sampling function
                      -> [(t, a)]                    -- ^ List of domain and function samples
samplePairs x x' i f = map (\s -> (s, f s)) [x, (x + i) .. x']

-- | Given a smoothing facor and a list of signal samples, compute the exponentially smoothed
-- signal. The smoothing parameter must be greater than zero and less than one.
ewma :: Double -> [Double] -> [Double]
ewma _ []     = []
ewma _ (x:[]) = [x]
ewma a (x:xs) = reverse $ foldl' f [x] xs
    where f m@(x':xs') n = ((a * n) + ((1 - a) * x')):m

-- | Given a smoothing factor and a list of signal samples, compute the exponentially weighted
-- moving average of the signal. The smoothing parameter must be greater than zero and less than
-- one. The average of an empty list is defined as zero.
ewma' :: Double -> [Double] -> Double
ewma' _ []     = 0
ewma' _ (x:[]) = x
ewma' a (x:xs) = foldl' f x xs
    where f m n = (a * n) + ((1 - a) * m)

-- | Compute the median of a list. The median of the empty list is zero.
median :: (Ord a, Fractional a) => [a] -> a
median [] = 0
median xs
    | odd l  = xs' !! m
    | even l = ((xs' !! m) + (xs' !! (m + 1))) / 2
        where l = length xs
              m = div (length xs) 2
              xs' = sort xs

-- | Compute the mode of a list. The mode of the empty list is zero. Ties are resolved with the
-- 'arithmeticMean' function.
mode :: [Double] -> Double
mode [] = 0
mode xs = arithmeticMean ties
    where freq = M.toList (foldl (\m x -> M.insertWith (+) x 1 m) M.empty xs)
          rank = sortBy (\(_, a) (_, b) -> compare b a) freq
          ties = map fst $ filter (\(_, a) -> a == snd (head rank)) rank

-- | Compute the mode of a list. The mode of the empty list is the empty list. In the case of a tie,
-- a list of all tying elements is returned.
genericMode :: Ord a => [a] -> [a]
genericMode [] = []
genericMode xs =  map fst $ filter (\(_, a) -> a == snd (head rank)) rank
    where freq = M.toList (foldl (\m x -> M.insertWith (+) x 1 m) M.empty xs)
          rank = sortBy (\(_, a) (_, b) -> compare b a) freq
