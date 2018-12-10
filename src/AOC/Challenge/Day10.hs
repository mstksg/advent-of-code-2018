{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : AOC.Challenge.Day10
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day10 (
    day10a
  , day10b
  , centralize
  ) where

import           AOC.Common     (clearOut)
import           AOC.Solver     ((:~>)(..))
import           Control.Monad  (guard)
import           Data.Bifunctor (second)
import           Data.Char      (isDigit)
import           Data.Foldable  (toList, foldMap)
import           Data.List      (unfoldr, uncons)
import           Data.Map       (Map)
import           Data.Maybe     (catMaybes)
import           Data.Semigroup (Min(..), Max(..), Sum(..))
import           Data.Set       (Set)
import           Linear         (V2(..))
import           Text.Heredoc   (here)
import qualified Data.Map       as M
import qualified Data.Set       as S
import qualified Linear         as L

type Point   = V2 Double
type Lattice = V2 Int

-- | Shift so that centroid is at zero
centralize :: [Point] -> [Point]
centralize ps = map (subtract mean) ps
  where
    mean = (/ len) <$> tot
    (Sum tot, Sum len) = foldMap (\x -> (Sum x, Sum 1)) ps

-- | Multiply and find trace
traceMul :: [Point] -> [Point] -> Double
traceMul xs ys = sum $ zipWith L.dot xs ys

findWord
    :: [Point]              -- ^ velocities
    -> [Point]              -- ^ points
    -> (Set Lattice, Int)   -- ^ points in word, and # of iterations
findWord vs xs = (S.fromList ((map . fmap) round final), round t)
  where
    vs'   = centralize vs
    xs'   = centralize xs
    t     = negate $ traceMul xs' vs' / traceMul vs' vs'
    final = zipWith (\v x -> x + t L.*^ v) vs xs

day10a :: ([Point], [Point]) :~> Set Lattice
day10a = MkSol
    { sParse = fmap unzip . traverse parsePoint . lines
    , sShow  = parseResult
    , sSolve = Just . fst . uncurry findWord
    }

day10b :: ([Point], [Point]) :~> Int
day10b = MkSol
    { sParse = fmap unzip . traverse parsePoint . lines
    , sShow  = show
    , sSolve = Just . snd . uncurry findWord
    }

boundingBox :: [Lattice] -> V2 Lattice
boundingBox ps = V2 xMin yMin `V2` V2 xMax yMax
  where
    (Min xMin, Min yMin, Max xMax, Max yMax) = flip foldMap ps $ \(V2 x y) ->
        (Min x, Min y, Max x, Max y)

-- | New solution: Parse the set of points into a string, based on
-- 'letterforms'.
parseResult :: Set Lattice -> String
parseResult ps = case M.lookup letter letterMap of
    Nothing -> []
    Just c  -> c : parseResult rest
  where
    origin `V2` _  = boundingBox (toList ps)
    shiftedPs      = subtract origin `S.map` ps
    (letter, rest) = S.partition (\(V2 x y) -> x < 6 && y < 10) shiftedPs

parsePoint :: String -> Maybe (Point, Point)
parsePoint xs = case map read . words . clearOut p $ xs of
    [x,y,vx,vy] -> Just (V2 vx vy, V2 x y)
    _           -> Nothing
  where
    p '-' = False
    p c   = not $ isDigit c

-- | A map of a set of "on" points (for a 6x10 grid) to the letter
-- they represent
letterMap :: Map (Set Lattice) Char
letterMap = M.fromList
          . unfoldr (uncurry peel)
          . second (filter (not . null) . lines)
          $ letterforms
  where
    peel :: String -> [String] -> Maybe ((Set Lattice, Char), (String, [String]))
    peel cs ls = do
      (d,ds) <- uncons cs
      let (m,ms) = unzip . map (splitAt 6) $ ls
          pointMap = S.fromList
                   . foldMap catMaybes
                   . zipWith (\j -> zipWith (\i c -> V2 i j <$ guard (c == '#'))
                                            [0..]
                             )
                             [0..]
                   $ m
      pure ((pointMap, d), (ds, ms))

-- | All known letterforms.  Based on
-- <https://gist.github.com/usbpc/5fa0be48ad7b4b0594b3b8b029bc47b4>.
letterforms :: (String, String)
letterforms = ("ABCEFGHJKLNPRXZ",[here|
..##..#####..####.############.####.#....#...####....##.....#....######.#####.#....#######
.#..#.#....##....##.....#.....#....##....#....#.#...#.#.....##...##....##....##....#.....#
#....##....##.....#.....#.....#.....#....#....#.#..#..#.....##...##....##....#.#..#......#
#....##....##.....#.....#.....#.....#....#....#.#.#...#.....#.#..##....##....#.#..#.....#.
#....######.#.....#####.#####.#.....######....#.##....#.....#.#..######.#####...##.....#..
#######....##.....#.....#.....#..####....#....#.##....#.....#..#.##.....#..#....##....#...
#....##....##.....#.....#.....#....##....#....#.#.#...#.....#..#.##.....#...#..#..#..#....
#....##....##.....#.....#.....#....##....##...#.#..#..#.....#...###.....#...#..#..#.#.....
#....##....##....##.....#.....#...###....##...#.#...#.#.....#...###.....#....##....##.....
#....######..####.#######......###.##....#.###..#....########....##.....#....##....#######
|])
