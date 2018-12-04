{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2018.Challenge.Day04
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC2018.Solver" for the types used in this module!
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

module AOC2018.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           Data.Ord
import           AOC2018.Prelude
import           Control.Lens
import           Data.Time
import qualified Data.Map        as M

data Action = AShift Int
            | ASleep
            | AWake
  deriving (Show, Eq, Ord)

data Stamp = S { _sMin :: Integer }
           -- _sDay :: Day
           --     , _sMin :: Int
           -- _sYear :: Int, _sMonth :: Int, _sDay :: Int, _sHour :: Int, _sMin :: Int  }
  deriving (Show, Eq, Ord)

parseLine :: String -> Maybe (Stamp, Action)
parseLine (first (map read) . splitAt 5 . words . clearOut (not . isAlphaNum) -> ([y,m,d,h,mi],rest))
    = (S ((toModifiedJulianDay fullDay) * (24 * 60) + fullMin),) <$> a
  where
    fullDay = fromGregorian y (fromIntegral m) (fromIntegral d)
    fullMin = h * 60 + mi
    a = case rest of
      "falls":"asleep":_ -> Just ASleep
      "wakes":"up":_     -> Just AWake
      "Guard":n:_        -> AShift <$> readMaybe n
      _                  -> Nothing
parseLine _ = Nothing

data Status = StatSleep Stamp
            | StatAwak

simulate :: Map Stamp Action -> State (Map Int (Map Integer Int), Int, Status) ()
simulate = void . M.traverseWithKey go
  where
    go :: Stamp -> Action -> State (Map Int (Map Integer Int), Int, Status) ()
    go st = \case
      AShift i -> modify $ set _3 StatAwak
                         . set _2 i
      ASleep   -> modify $ set _3 $ StatSleep st
      AWake    -> modify $ \(m, i, StatSleep st0) ->
                    let m' = M.insertWith (M.unionWith (+)) i (minutesList st0 st) m
                    in  (m', i, StatAwak)
                        
-- [1518-07-18 00:55] falls asleep
                        
minutesList :: Stamp -> Stamp -> Map Integer Int
minutesList (S s1) (S s2) = freqs $ map (`mod` 60) [s1 .. s2 - 1]

-- maxVal :: Map a b -> (a, b)
-- maxVal = map (\(x, xs) -> (x, M.toList

getGuard :: Map Int (Map Integer Int) -> (Int, Map Integer Int)
getGuard = maximumBy (comparing (sum . snd)) . M.toList

mostSeen :: (Int, Map Integer Int) -> Int
mostSeen (g, ms) = g * fromIntegral maxMin
  where
    maxMin = fst . maximumBy (comparing snd) . M.toList $ ms

day04a :: Map Stamp Action :~> Int
day04a = MkSol
    { sParse = fmap M.fromList . traverse parseLine . lines
    , sShow  = show
    , sSolve = Just . mostSeen . getGuard . view _1 . flip execState (M.empty, -1, StatAwak) . simulate
    }

day04b :: _ :~> Int
day04b = MkSol
    { sParse = fmap M.fromList . traverse parseLine . lines
    , sShow  = show
    , sSolve = Just . uncurry (*) . second fromIntegral . getGuard2 . view _1 . flip execState (M.empty, -1, StatAwak) . simulate
    }

getGuard2 :: Map Int (Map Integer Int) -> (Int, Integer)
getGuard2 = second fst . maximumBy (comparing (snd . snd)) . (map . second) floop . M.toList
  where
    floop = maximumBy (comparing snd) . M.toList

