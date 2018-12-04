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

import           AOC2018.Prelude
import           Control.Lens
import           Data.Finite
import           Data.Time
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M

newtype Time = T { _tRawMin :: Integer }
  deriving (Show, Eq, Ord, Num)

data Action = AShift Guard
            | ASleep
            | AWake
  deriving (Show, Eq, Ord)

newtype Guard = G { _gId :: Int }
  deriving (Show, Eq, Ord, Num)

makeLenses ''Guard

type Minute = Finite 60

parseLine :: String -> Maybe (Time, Action)
parseLine (first (map read) . splitAt 5 . words . clearOut (not . isAlphaNum) -> ([y,m,d,h,mi],rest))
    = (T (toModifiedJulianDay fullDay * 24 * 60 + fullMin),) <$> a
  where
    fullDay = fromGregorian y (fromIntegral m) (fromIntegral d)
    fullMin = h * 60 + mi
    a = case rest of
      "falls":"asleep":_ -> Just ASleep
      "wakes":"up":_     -> Just AWake
      "Guard":n:_        -> AShift . G <$> readMaybe n
      _                  -> Nothing
parseLine _ = Nothing

data Status = StatSleep Time
            | StatAwake

data ProgState = PS { _psTimeCard :: Map Guard (Map Minute Int)
                    , _psGuard    :: Guard
                    , _psStatus   :: Status
                    }

makeLenses ''ProgState

simulate :: Map Time Action -> State ProgState ()
simulate = void . M.traverseWithKey go
  where
    go :: Time -> Action -> State ProgState ()
    go st = \case
      AShift i -> modify $ set psStatus StatAwake
                         . set psGuard i
      ASleep   -> modify $ set psStatus (StatSleep st)
      AWake    -> modify $ \(PS m i (StatSleep st0)) ->
        let m' = M.insertWith (M.unionWith (+)) i (minutesList st0 st) m
        in  PS m' i StatAwake
                        
minutesList :: Time -> Time -> Map Minute Int
minutesList (T s1) (T s2) = freqs . map modulo $ [s1 .. s2 - 1]

mostSeen :: (Guard, Map Minute Int) -> Maybe Int
mostSeen (G g, ms) = (g *) . fromIntegral . fst <$> maxMinute
  where
    maxMinute = maximumVal ms

day04a :: Map Time Action :~> Int
day04a = MkSol
    { sParse = fmap M.fromList . traverse parseLine . lines
    , sShow  = show
    , sSolve = (mostSeen =<<)
             . maximumByVal (comparing sum)
             . view psTimeCard
             . flip execState (PS M.empty (-1) StatAwake)
             . simulate
    }

day04b :: Map Time Action :~> Int
day04b = MkSol
    { sParse = fmap M.fromList . traverse parseLine . lines
    , sShow  = show
    , sSolve = fmap (\(G i, (m, _)) -> i * fromIntegral m)
             . maximumByVal (comparing snd)
             . M.mapMaybe maximumVal
             . view psTimeCard
             . flip execState (PS M.empty (-1) StatAwake)
             . simulate
    }
