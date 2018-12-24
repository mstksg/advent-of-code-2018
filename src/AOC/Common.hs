{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
  , loopMaybe
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
  -- * 2D Maps
  , Point
  , cardinalNeighbs
  , fullNeighbs
  , mannDist
  , memoPoint
  , boundingBox
  , boundingBox'
  , parseAsciiMap
  , asciiGrid
  , ScanPoint(..)
  , displayAsciiMap
  ) where

import           AOC.Util
import           Control.Lens
import           Control.Parallel.Strategies
import           Data.Finite
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.List.NonEmpty                 (NonEmpty)
import           Data.Map                           (Map)
import           Data.Map.NonEmpty                  (NEMap)
import           Data.MemoCombinators               (Memo)
import           Data.Ord
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           GHC.TypeNats
import           Linear                             (V2(..), _x, _y)
import qualified Control.Foldl                      as F
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Map                           as M
import qualified Data.Map.NonEmpty                  as NEM
import qualified Data.MemoCombinators               as Memo
import qualified Data.Set                           as S
import qualified Data.Vector.Generic.Sized.Internal as SVG

-- | Strict (!!)
(!!!) :: [a] -> Int -> a
[] !!! _ = error "Out of range"
(x:_ ) !!! 0 = x
(x:xs) !!! n = x `seq` (xs !!! (n - 1))

-- | Iterate until a 'Nothing' is produced
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x0 = x0 : unfoldr (fmap dup . f) x0

-- | Apply function until 'Nothing' is produced, and return last produced
-- value.
loopMaybe
    :: (a -> Maybe a)
    -> a
    -> a
loopMaybe f = go
  where
    go !x = case f x of
      Nothing -> x
      Just !y -> go y


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

-- | 2D Coordinate
type Point = V2 Int

-- | Find the minimum and maximum x and y from a collection of points.
--
-- Returns @'V2' (V2 xMin yMin) (V2 xMax yMax)@.
boundingBox :: Foldable1 f => f Point -> V2 Point
boundingBox ps = V2 xMin yMin `V2` V2 xMax yMax
  where
    (Min xMin, Min yMin, Max xMax, Max yMax) = flip foldMap1 ps $ \(V2 x y) ->
        (Min x, Min y, Max x, Max y)

-- | A version of 'boundingBox' that works for normal possibly-empty lists.
boundingBox' :: Foldable f => f Point -> Maybe (V2 Point)
boundingBox' = fmap boundingBox . NE.nonEmpty . toList

cardinalNeighbs :: Point -> [Point]
cardinalNeighbs p = (p +) <$> [ V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0 ]

fullNeighbs :: Point -> [Point]
fullNeighbs p = [ p + V2 dx dy
                | dx <- [-1 .. 1]
                , dy <- if dx == 0 then [-1,1] else [-1..1]
                ]


memoPoint :: Memo Point
memoPoint = Memo.wrap (uncurry V2) (\(V2 x y) -> (x, y)) $
                Memo.pair Memo.integral Memo.integral

mannDist :: (Foldable f, Num a, Num (f a)) => f a -> f a -> a
mannDist x y = sum . abs $ x - y


-- | It's 'Point', but with a newtype wrapper so we have an 'Ord' that
-- sorts by y first, then x
newtype ScanPoint = SP { _getSP :: Point }
  deriving (Eq, Show, Num)

instance Ord ScanPoint where
    compare = comparing (view _y . _getSP)
           <> comparing (view _x . _getSP)

parseAsciiMap
    :: (Char -> Maybe a)
    -> String
    -> Map Point a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) M.singleton

asciiGrid :: IndexedFold Point String Char
asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

displayAsciiMap
    :: Char             -- ^ default tile
    -> Map Point Char   -- ^ tile map
    -> String
displayAsciiMap d (NEM.IsNonEmpty mp) = unlines
    [ [ NEM.findWithDefault d (V2 x y) mp
      | x <- [xMin .. xMax]
      ]
    | y <- [yMin .. yMax]
    ]
  where
    V2 xMin yMin `V2` V2 xMax yMax = boundingBox $ NEM.keysSet mp
displayAsciiMap _ _ = ""




type instance Index   (SVG.Vector v n a) = Int
type instance IxValue (SVG.Vector v n a) = a

instance (Ixed (v a), Index (v a) ~ Int, IxValue (v a) ~ a) => Ixed (SVG.Vector v n a) where
    ix i f (SVG.Vector v) = SVG.Vector <$> ix i f v
