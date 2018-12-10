-- |
-- Module      : AOC2018.Challenge.Day10
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC2018.Solver" for the types used in this module!

module AOC2018.Challenge.Day10 (
    day10a
  , day10b
  ) where

import           AOC2018.Solver     ((:~>)(..))
import           AOC2018.Util       (clearOut)
import           Data.Char          (isDigit)
import           Data.Foldable      (toList)
import           Data.Semigroup     (Min(..), Max(..))
import           Data.Set           (Set)
import           Linear             (V2(..))
import qualified Data.Set           as S

type Point = V2 Int

simulate
    :: [Point]        -- ^ velocities
    -> [Point]        -- ^ points
    -> [Point]        -- ^ new points
simulate = zipWith (+)

boundingBox :: [Point] -> V2 Point
boundingBox ps = V2 xMin yMin `V2` V2 xMax yMax
  where
    (Min xMin, Min yMin, Max xMax, Max yMax) = flip foldMap ps $ \(V2 x y) ->
        (Min x, Min y, Max x, Max y)

clusterArea :: [Point] -> Int
clusterArea (boundingBox -> V2 mins maxs) = product $ maxs - mins

findWord
    :: [Point]            -- ^ velocities
    -> [Point]            -- ^ points
    -> (Set Point, Int)   -- ^ points in word, and # of iterations
findWord vs xs0 = go 0 (clusterArea xs0) xs0
  where
    go :: Int -> Int -> [Point] -> (Set Point, Int)
    go !i !area !xs
        | area' > area = (S.fromList xs, i)
        | otherwise    = go (i + 1) area' xs'
      where
        xs'   = simulate vs xs
        area' = clusterArea xs'

day10a :: ([Point], [Point]) :~> Set Point
day10a = MkSol
    { sParse = fmap unzip . traverse parsePoint . lines
    , sShow  = display
    , sSolve = Just . fst . uncurry findWord
    }

day10b :: ([Point], [Point]) :~> Int
day10b = MkSol
    { sParse = fmap unzip . traverse parsePoint . lines
    , sShow  = show
    , sSolve = Just . snd . uncurry findWord
    }

display :: Set Point -> String
display ps = unlines [ [ if V2 x y `S.member` ps then '#' else '.'
                       | x <- [xMin .. xMax]
                       ]
                     | y <- [yMin .. yMax]
                     ]
  where
    V2 xMin yMin `V2` V2 xMax yMax = boundingBox (toList ps)

parsePoint :: String -> Maybe (Point, Point)
parsePoint xs = case map read . words . clearOut p $ xs of
    [x,y,vx,vy] -> Just (V2 vx vy, V2 x y)
    _           -> Nothing
  where
    p '-' = False
    p c   = not $ isDigit c

