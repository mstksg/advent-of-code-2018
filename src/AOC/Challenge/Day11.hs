{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day11 where

-- module AOC.Challenge.Day11 (
--     day11a
--   , day11b
--   ) where

import           AOC.Prelude
import           Control.Lens
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import qualified Data.Map.NonEmpty  as NEM
import qualified Data.Set           as S
import qualified Data.Set.NonEmpty  as NES
import qualified Data.Vector        as V
import qualified Linear             as L

type Point = V2 Int

powerLevel :: Int -> Point -> Int
powerLevel sid (V2 x y) = pl3 - 5
  where
    rid = x + 10
    pl0 = rid * y
    pl1 = pl0 + sid
    pl2 = pl1 * rid
    pl3 = (pl2 `div` 100) `mod` 10

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
findMaxAny mp = fst . maximumBy (comparing snd) $
                        [ ((p, n), fromPartialSums ps p n)
                        | !n <- [1 .. 300]
                        , !p <- range (V2 1 1, V2 (300 - n + 1) (300 - n + 1))
                        ]
  where
    ps = partialSums mp

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
    go p0@(V2 x0 y0) v = v + adds + M.findWithDefault 0 (p0 - V2 1 1) pasu
      where
        adds = sum [ mp M.! p
                   | p <- range (V2 x0 1 , V2 x0       (y0 - 1))
                       ++ range (V2 1  y0, V2 (x0 - 1) y0      )
                   ]
