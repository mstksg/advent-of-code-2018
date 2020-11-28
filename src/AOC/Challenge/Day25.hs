-- |
-- Module      : AOC.Challenge.Day25
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 25.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day25 (
    day25a
  ) where

import           AOC.Common      (mannDist, clearOut)
import           AOC.Solver      ((:~>)(..))
import           Data.Char       (isDigit)
import           Data.Graph      (Graph)
import           Data.Witherable (mapMaybe)
import           Linear          (V4(..))
import qualified Data.Graph      as G

constellationGraph :: [V4 Int] -> Graph
constellationGraph xs = g
  where
    (g, _, _) = G.graphFromEdges (map collect xs)
    collect x = ((), x, filter ((<= 3) . mannDist x) xs)

day25a :: [V4 Int] :~> Int
day25a = MkSol
    { sParse = Just . parse25
    , sShow  = show
    , sSolve = Just . length . G.scc . constellationGraph
    }

parse25 :: String -> [V4 Int]
parse25 = mapMaybe (go . map read . words . clearOut d) . lines
  where
    d '-' = False
    d c   = not (isDigit c)
    go [x,y,z,r] = Just $ V4 x y z r
    go _         = Nothing

