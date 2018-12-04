-- |
-- Module      : AOC2018.Util
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Assorted utility functions and orphans used for solutions.
--

module AOC2018.Util (
    strip
  , iterateMaybe
  , (!!!)
  , dup
  , scanlT
  , scanrT
  , eitherToMaybe
  , maybeToEither
  , firstRepeated
  , freqs
  , perturbations
  , findMaybe
  , clearOut
  , maximumVal
  , maximumByVal
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Except
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Map             (Map)
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Text            as T

-- | Strict (!!)
(!!!) :: [a] -> Int -> a
[] !!! _ = error "Out of range"
(x:_ ) !!! 0 = x
(x:xs) !!! n = x `seq` (xs !!! (n - 1))

-- | Strip trailing and leading whitespace.
strip :: String -> String
strip = T.unpack . T.strip . T.pack

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

-- | Convert an 'Either' into a 'Maybe', or any 'Alternative' instance,
-- forgetting the error value.
eitherToMaybe :: Alternative m => Either e a -> m a
eitherToMaybe = either (const empty) pure

-- | Convert a 'Maybe' into an 'Either', or any 'MonadError' instance, by
-- providing an error value in case 'Nothing' was given.
maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither e = maybe (throwError e) pure

-- | Lazily find the first repeated item.
firstRepeated :: Ord a => [a] -> Maybe a
firstRepeated = go S.empty
  where
    go seen (x:xs)
      | x `S.member` seen = Just x
      | otherwise         = go (x `S.insert` seen) xs
    go _ []     = Nothing

-- | Build a frequency map
freqs :: Ord a => [a] -> Map a Int
freqs = M.fromListWith (+) . map (,1)

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

-- | Like 'find', but instead of taking an @a -> Bool@, takes an @a ->
-- Maybe b@ and returns the first success.
findMaybe
    :: Foldable t
    => (a -> Maybe b)
    -> t a
    -> Maybe b
findMaybe p = asum . map p . toList

-- | Clear out characters not matching a predicate
clearOut :: (Char -> Bool) -> String -> String
clearOut p = map $ \c -> if p c then ' '
                                else c

-- | Get the key-value pair corresponding to the maximum value in the map
maximumVal :: Ord b => Map a b -> Maybe (a, b)
maximumVal = maximumByVal compare

-- | Get the key-value pair corresponding to the maximum value in the map,
-- with a custom comparing function.
--
-- > 'maximumVal' == 'maximumByVal' 'compare'
maximumByVal :: (b -> b -> Ordering) -> Map a b -> Maybe (a, b)
maximumByVal c = fmap (maximumBy (c `on` snd))
               . NE.nonEmpty
               . M.toList

