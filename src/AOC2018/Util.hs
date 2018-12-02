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
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.List
import           Data.Map             (Map)
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

dup :: a -> (a, a)
dup x = (x, x)

-- | 'scanl' generalized to all 'Traversable'.
scanlT :: Traversable t => (b -> a -> b) -> b -> t a -> t b
scanlT f z = snd . mapAccumL (\x -> dup . f x) z

-- | 'scanr' generalized to all 'Traversable'.
scanrT :: Traversable t => (a -> b -> b) -> b -> t a -> t b
scanrT f z = snd . mapAccumR (\x -> dup . flip f x) z

eitherToMaybe :: Alternative m => Either e a -> m a
eitherToMaybe = either (const empty) pure

maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither e = maybe (throwError e) pure

-- | Lazily find the first repeated item.
firstRepeated :: Ord a => [a] -> Maybe a
firstRepeated = go S.empty
  where
    go s (x:xs)
      | x `S.member` s = Just x
      | otherwise      = go (x `S.insert` s) xs
    go _ []     = Nothing

-- | Build a frequency map
freqs :: Ord a => [a] -> Map a Int
freqs = M.fromListWith (+) . map (,1)
