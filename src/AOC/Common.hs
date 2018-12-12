-- |
-- Module      : AOC.Common
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Common functions for solutions
--

module AOC.Common (
    module AOC.Util
  , iterateMaybe
  , (!!!)
  , dup
  , scanlT
  , scanrT
  , firstRepeated
  , freqs
  , perturbations
  , clearOut
  , maximumVal
  , maximumValBy
  , minimumVal
  , minimumValBy
  , maximumValNE
  , maximumValByNE
  , minimumValNE
  , minimumValByNE
  , deleteFinite
  , foldMapPar
  , foldMapPar1
  , meanVar
  ) where

import           AOC.Util
import           Control.Lens
import           Control.Parallel.Strategies
import           Data.Finite
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Map                    (Map)
import           Data.Map.NonEmpty           (NEMap)
import           Data.Semigroup
import           GHC.TypeNats
import qualified Control.Foldl               as F
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as M
import qualified Data.Map.NonEmpty           as NEM
import qualified Data.Set                    as S

-- | Strict (!!)
(!!!) :: [a] -> Int -> a
[] !!! _ = error "Out of range"
(x:_ ) !!! 0 = x
(x:xs) !!! n = x `seq` (xs !!! (n - 1))

-- | Iterate until a 'Nothing' is produced
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x0 = x0 : unfoldr (fmap dup . f) x0

-- | A tuple of the same item twice
dup :: a -> (a, a)
dup x = (x, x)

-- | 'scanl' generalized to all 'Traversable'.
scanlT :: Traversable t => (b -> a -> b) -> b -> t a -> t b
scanlT f z = snd . mapAccumL (\x -> dup . f x) z

-- | 'scanr' generalized to all 'Traversable'.
scanrT :: Traversable t => (a -> b -> b) -> b -> t a -> t b
scanrT f z = snd . mapAccumR (\x -> dup . flip f x) z

-- | Lazily find the first repeated item.
firstRepeated :: Ord a => [a] -> Maybe a
firstRepeated = go S.empty
  where
    go seen (x:xs)
      | x `S.member` seen = Just x
      | otherwise         = go (x `S.insert` seen) xs
    go _ []     = Nothing

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

-- | Collect all possible single-item perturbations from a given
-- perturbing function.
--
-- > perturbations (\i -> [i - 1, i + 1]) [0,10,100]
--      == [ [-1,10,100]
--         , [ 1,10,100]
--         , [ 0, 9,100]
--         , [ 0,11,100]
--         , [ 0,10, 99]
--         , [ 0,10,101]
--         ]
perturbations
    :: (a -> [a])
    -> [a]
    -> [[a]]
perturbations f xs = do
    i <- [0 .. length xs - 1]
    xs & ix i %%~ f

-- | Clear out characters not matching a predicate
clearOut :: (Char -> Bool) -> String -> String
clearOut p = map $ \c -> if p c then ' '
                                else c

-- | Get the key-value pair corresponding to the maximum value in the map
maximumVal :: Ord b => Map a b -> Maybe (a, b)
maximumVal = maximumValBy compare

-- | Get the key-value pair corresponding to the maximum value in the map,
-- with a custom comparing function.
--
-- > 'maximumVal' == 'maximumValBy' 'compare'
maximumValBy :: (b -> b -> Ordering) -> Map a b -> Maybe (a, b)
maximumValBy c = fmap (maximumBy (c `on` snd))
               . NE.nonEmpty
               . M.toList

-- | Get the key-value pair corresponding to the minimum value in the map,
-- with a custom comparing function.
--
-- > 'minimumVal' == 'minimumValBy' 'compare'
minimumValBy :: (b -> b -> Ordering) -> Map a b -> Maybe (a, b)
minimumValBy c = fmap (minimumBy (c `on` snd))
               . NE.nonEmpty
               . M.toList

-- | Get the key-value pair corresponding to the minimum value in the map
minimumVal :: Ord b => Map a b -> Maybe (a, b)
minimumVal = minimumValBy compare

-- | Version of 'maximumValBy' for nonempty maps.
maximumValByNE :: (b -> b -> Ordering) -> NEMap a b -> (a, b)
maximumValByNE c = maximumBy (c `on` snd)
                 . NEM.toList

-- | Version of 'maximumVal' for nonempty maps.
maximumValNE :: Ord b => NEMap a b -> (a, b)
maximumValNE = maximumValByNE compare

-- | Version of 'minimumValBy' for nonempty maps.
minimumValByNE :: (b -> b -> Ordering) -> NEMap a b -> (a, b)
minimumValByNE c = minimumBy (c `on` snd)
                 . NEM.toList

-- | Version of 'minimumVal' for nonempty maps.
minimumValNE :: Ord b => NEMap a b -> (a, b)
minimumValNE = minimumValByNE compare

-- | Delete a potential value from a 'Finite'.
deleteFinite
    :: KnownNat n
    => Finite (n + 1)
    -> Finite (n + 1)
    -> Maybe (Finite n)
deleteFinite n m = case n `cmp` m of
    LT -> unshift m
    EQ -> Nothing
    GT -> strengthen m

-- | 'foldMap', but in parallel.
foldMapPar :: Monoid b => (a -> b) -> [a] -> b
foldMapPar f = runEval . fmap mconcat . traverse (rpar . f)

-- | 'foldMap1', but in parallel.
foldMapPar1 :: Semigroup b => (a -> b) -> NonEmpty a -> b
foldMapPar1 f = runEval . fmap sconcat . traverse (rpar . f)

-- | 'F.Fold' for computing mean and variance
meanVar :: Fractional a => F.Fold a (a, a)
meanVar = do
    n  <- fromIntegral <$> F.length
    x  <- F.sum
    x2 <- lmap (^ (2 :: Int)) F.sum
    pure $ let μ  = x / n
               σ2 = x2 / n - μ * μ
           in  (μ, σ2)
