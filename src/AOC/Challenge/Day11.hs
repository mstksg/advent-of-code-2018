-- |
-- Module      : AOC.Challenge.Day11
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day11 (
    day11a
  , day11b
  ) where

import           AOC.Common
import           AOC.Solver      ((:~>)(..))
import           Control.DeepSeq (force)
import           Data.Foldable   (maximumBy)
import           Data.Ix         (range)
import           Data.Map        (Map)
import           Data.Maybe      (catMaybes)
import           Data.Maybe
import           Data.Ord        (comparing)
import           Data.Semigroup
import           Linear          (V2(..))
import           Text.Read       (readMaybe)
import qualified Data.Map        as M
import qualified Data.Set        as S

type Point = V2 Int

powerLevel :: Int -> Point -> Int
powerLevel sid (V2 x y) = hun ((rid * y + sid) * rid) - 5
  where
    hun = (`mod` 10) . (`div` 100)
    rid = x + 10

findMaxThree :: Map Point Int -> Point
findMaxThree mp = fst
                . maximumBy (comparing snd)
                . map (\x -> (x, go x))
                $ range (V2 1 1, V2 298 298)
  where
    go p = sum [ mp M.! (p + shift)
               | shift <- range (V2 0 0, V2 2 2)
               ]

mkMap :: Int -> Map Point Int
mkMap i = M.fromSet (powerLevel i) . S.fromList $ range (V2 1 1, V2 300 300)

day11a :: Int :~> Point
day11a = MkSol
    { sParse = readMaybe
    , sShow  = \(V2 x y) -> show x ++ "," ++ show y
    , sSolve = Just . findMaxThree . mkMap
    }

day11b :: Int :~> (Point, Int)
day11b = MkSol
    { sParse = readMaybe
    , sShow  = \(V2 x y, s) -> show x ++ "," ++ show y ++ "," ++ show s
    , sSolve = Just . findMaxAny . mkMap
    }

findMaxAny :: Map Point Int -> (Point, Int)
findMaxAny mp = (\(Arg _ x) -> x) . getMax . fromJust
              . (foldMapPar . foldMapPar) id
              $ [ [ Just (Max (Arg s (p, n)))
                  | !p <- range (V2 1 1, V2 (300 - n + 1) (300 - n + 1))
                  , let !s = fromPartialSums ps p n
                  ]
                | !n <- [1 .. 300]
                ]
  where
    !ps = partialSums mp

fromPartialSums :: Map Point Int -> Point -> Int -> Int
fromPartialSums ps (subtract (V2 1 1)->p) n = sum . catMaybes $
    [            M.lookup p            ps
    ,            M.lookup (p + V2 n n) ps
    , negate <$> M.lookup (p + V2 0 n) ps
    , negate <$> M.lookup (p + V2 n 0) ps
    ]

partialSums :: Map Point Int -> Map Point Int
partialSums mp = force pasu
  where
    pasu = M.mapWithKey go mp
    go p0 v = (+ v) . sum . catMaybes $
      [ negate <$> M.lookup (p0 - V2 1 1) pasu
      ,            M.lookup (p0 - V2 1 0) pasu
      ,            M.lookup (p0 - V2 0 1) pasu
      ]
