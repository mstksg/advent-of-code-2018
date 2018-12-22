-- |
-- Module      : AOC.Challenge.Day18
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day18 (
    day18a
  , day18b
  ) where

import           AOC.Common    (Point, (!!!), parseAsciiMap, fullNeighbs)
import           AOC.Solver    ((:~>)(..))
import           Control.Lens  (folded, lengthOf, only)
import           Control.Monad (mfilter)
import           Data.Map      (Map)
import           Data.Maybe    (mapMaybe)
import qualified Data.Map      as M

data Terrain = TOpen
             | TTree
             | TYard
  deriving (Show, Eq, Ord)

type World = Map Point Terrain

stepMap :: World -> World
stepMap mp = M.mapWithKey go mp
  where
    go :: Point -> Terrain -> Terrain
    go p = \case
        TOpen
          | neighbCount TTree >= 3 -> TTree
          | otherwise              -> TOpen
        TTree
          | neighbCount TYard >= 3 -> TYard
          | otherwise              -> TTree
        TYard
          | neighbCount TYard >= 1
         && neighbCount TTree >= 1 -> TYard
          | otherwise              -> TOpen
      where
        neighbCount t = length
                      . mapMaybe (mfilter (== t) . (`M.lookup` mp))
                      . fullNeighbs
                      $ p

day18a :: World :~> Int
day18a = MkSol
    { sParse = Just . parseForest
    , sShow  = show
    , sSolve = \m0 -> Just $
        let mp = iterate stepMap m0 !!! 10
        in  lengthOf (folded . only TTree) mp
              * lengthOf (folded . only TYard) mp
    }

findLoop
    :: World
    -> (Int, Int)   -- ^ time to loop, loop size
findLoop w0 = go 1 (M.singleton w0 0) w0
  where
    go !i !seen !w = case M.lookup w' seen  of
        Nothing  -> go (i + 1) (M.insert w' i seen) w'
        Just ttl -> (ttl, i - ttl)
      where
        w' = stepMap w

stepN
    :: Int
    -> World
    -> World
stepN n m0 = goN extra . goN ttl $ m0
  where
    goN i = (!!! i) . iterate stepMap
    (ttl, loopSize) = findLoop m0
    extra           = (n - ttl) `mod` loopSize

day18b :: World :~> Int
day18b = MkSol
    { sParse = Just . parseForest
    , sShow  = show
    , sSolve = \m0 -> Just $
        let mp = stepN 1000000000 m0
        in  lengthOf (folded . only TTree) mp
              * lengthOf (folded . only TYard) mp
    }

parseForest :: String -> World
parseForest = parseAsciiMap $ \case
    '.' -> Just TOpen
    '|' -> Just TTree
    '#' -> Just TYard
    _   -> Nothing
