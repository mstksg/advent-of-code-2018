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

import           AOC.Common                       (Point, meanVar)
import           AOC.Solver                       ((:~>)(..))
import           Control.DeepSeq                  (force)
import           Data.Foldable                    (maximumBy)
import           Data.Ix                          (range)
import           Data.Map                         (Map)
import           Data.Maybe                       (catMaybes)
import           Data.Ord                         (comparing)
import           Data.Profunctor                  (dimap)
import           Linear                           (V2(..))
import           Text.Read                        (readMaybe)
import qualified Control.Foldl                    as F
import qualified Data.Map                         as M
import qualified Data.Set                         as S
import qualified Statistics.Distribution          as D
import qualified Statistics.Distribution.Binomial as D
import qualified Statistics.Distribution.Normal   as D

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
findMaxAny mp = fst $ go 1
  where
    go n
       | goOn > 0.001 = maximumBy (comparing snd) [((pMax, n), oMax), go (n + 1)]
       | otherwise    = ((pMax, n), oMax)
                      -- & traceShow (n, oMax, goOn)
      where
        (pMax, oMax) = maximumBy (comparing snd)
            [ (p, fromIntegral (fromSAT sat p n))
            | p <- range (V2 1 1, V2 (300 - n + 1) (300 - n + 1))
            ]
        goOn = sum
            [ probGreaterThan oMax n'
            | n' <- [n + 1 .. 300]
            ]
    !sat = summedAreaTable mp
    σ :: Double
    σ = sqrt $ ((4 + 5)**2)/12   -- stdev of uniform distribution between -5 and 4
    probGreaterThan o n
        | prob2 == 0 = 0
        | otherwise  = probIn2
      where
        n' = fromIntegral n
        distr2  = D.normalDistr (-(n' ** 2) * 0.5) (n' * σ)
        prob2   = D.complCumulative distr2 o
        numIn2  = ( (300 `div` n) ^ (2 :: Int)    -- we compensate for dependence
                  + (300 - n + 1)^(2 :: Int)
                  ) `div` 2
        probIn2 = 1 - D.probability (D.binomial numIn2 prob2) 0
                    -- this is technically not a bernoulli process,
                    -- because our items are dependent. but we fudge this
                    -- by tweaking the number of trials

fromSAT :: Map Point Int -> Point -> Int -> Int
fromSAT sat (subtract (V2 1 1)->p) n = sum . catMaybes $
    [            M.lookup p            sat
    ,            M.lookup (p + V2 n n) sat
    , negate <$> M.lookup (p + V2 0 n) sat
    , negate <$> M.lookup (p + V2 n 0) sat
    ]

summedAreaTable :: Map Point Int -> Map Point Int
summedAreaTable mp = force sat
  where
    sat = M.mapWithKey go mp
    go p0 v = (+ v) . sum . catMaybes $
      [ negate <$> M.lookup (p0 - V2 1 1) sat
      ,            M.lookup (p0 - V2 1 0) sat
      ,            M.lookup (p0 - V2 0 1) sat
      ]

-- | Debug: find the variance of the map at every square size
_chunkyVars :: Map Point Int -> Map Int Double
_chunkyVars mp = flip M.fromSet (S.fromList [1..300]) $ \n ->
    F.fold (dimap fromIntegral snd meanVar)
      [ fromSAT sat p n
      | p <- range (V2 1 1, V2 (300 - n + 1) (300 - n + 1))
      ]
  where
    !sat = summedAreaTable mp
