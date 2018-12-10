{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC2018.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC2018.Challenge.Day10 where
    -- day10a
  -- , day10b
  -- ) where

import           AOC2018.Prelude
import           Data.Semigroup.Foldable
import qualified Data.List.NonEmpty      as NE
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Linear                  as L

type Point = V2 Int

simulate :: [Point] -> [Point] -> [Point]
simulate = zipWith (+)

boundingBox :: [Point] -> V2 Point
boundingBox ps = V2 xMin yMin `V2` V2 xMax yMax
  where
    (Min xMin, Min yMin, Max xMax, Max yMax) = flip foldMap ps $ \(V2 x y) ->
        (Min x, Min y, Max x, Max y)

clusterArea :: [Point] -> Int
clusterArea (boundingBox -> V2 mins maxs) = product $ maxs - mins

findWord :: [Point] -> [Point] -> (Int, Set Point)
findWord vs xs0 = go 0 (clusterArea xs0) xs0
  where
    go :: Int -> Int -> [Point] -> (Int, Set Point)
    go i area xs
        | area' > area = (i, S.fromList xs)
        | otherwise    = go (i + 1) area' xs'
      where
        xs'   = simulate vs xs
        area' = clusterArea xs'

day10a :: ([Point], [Point]) :~> Set Point
day10a = MkSol
    { sParse = fmap unzip . traverse parsePoint . lines
    , sShow  = display
    , sSolve = Just . snd . uncurry findWord
    }

day10b :: ([Point], [Point]) :~> Int
day10b = MkSol
    { sParse = fmap unzip . traverse parsePoint . lines
    , sShow  = show
    , sSolve = Just . fst . uncurry findWord
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

