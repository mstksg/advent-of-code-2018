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
import           Advent.OCR     (parseLettersWith)
import           Control.Lens   (view)
import           Data.Char      (isDigit)
import           Data.Foldable  (foldMap)
import           Data.Maybe     (fromMaybe)
import           Data.Semigroup (Sum(..))
import           Data.Set       (Set)
import           Linear         (V2(..), _x, _y)
import qualified Data.Set       as S
import qualified Linear         as L

type Point   = V2 Double
type Lattice = V2 Int

-- | Shift so that centroid is at zero
centralize :: [Point] -> [Point]
centralize ps = map (subtract mean) ps
  where
    (Sum tot, Sum len) = foldMap (\x -> (Sum x, Sum 1)) ps
    mean               = tot L.^/ len

-- | Multiply and find trace
traceMul :: [Point] -> [Point] -> Double
traceMul xs ys = sum $ zipWith L.dot xs ys

findWord
    :: [Point]              -- ^ velocities
    -> [Point]              -- ^ points
    -> (Set Lattice, Int)   -- ^ points in word, and # of iterations
findWord (centralize->vs) (centralize->xs) =
    (S.fromList ((map . fmap) round final), round t)
  where
    t     = negate $ traceMul xs vs / traceMul vs vs
    final = zipWith (\v x -> x + t L.*^ v) vs xs

day10a :: ([Point], [Point]) :~> Set Lattice
day10a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

day10b :: ([Point], [Point]) :~> Int
day10b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

parsePoint :: String -> Maybe (Point, Point)
parsePoint xs = case map read . words . clearOut p $ xs of
    [x,y,vx,vy] -> Just (V2 vx vy, V2 x y)
    _           -> Nothing
  where
    p '-' = False
    p c   = not $ isDigit c
