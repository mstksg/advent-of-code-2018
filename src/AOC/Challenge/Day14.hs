module AOC.Challenge.Day14 (
    day14a
  , day14b
  ) where

import           AOC.Solver    ((:~>)(..))
import           Data.List     (tails, isPrefixOf)
import           Data.Maybe    (mapMaybe)
import           Text.Read     (readMaybe)
import qualified Data.Sequence as Seq

digitize :: Int -> [Int]
digitize ((`divMod` 10)->(x,y))
    | x == 0    = [y]
    | otherwise = [x,y]

-- | This is our lazily generated stream of chocolate practice numbers!
-- Items will be demanded as users ask for them.
--
-- Note that this is independent of the input numbers, so it can be
-- generated in advance and shared by all inputs.
--
-- (This is actually a futumorphism, but don't tell anyone)
chocolatePractice :: [Int]
chocolatePractice = 3 : 7 : go 0 1 (Seq.fromList [3,7])
  where
    go !p1 !p2 !tp = newDigits ++ go p1' p2' tp'
      where
        sc1 = tp `Seq.index` p1
        sc2 = tp `Seq.index` p2
        newDigits = digitize $ sc1 + sc2
        tp' = tp <> Seq.fromList newDigits
        p1' = (p1 + sc1 + 1) `mod` length tp'
        p2' = (p2 + sc2 + 1) `mod` length tp'

day14a :: Int :~> [Int]
day14a = MkSol
    { sParse = readMaybe
    , sShow  = concatMap show
    , sSolve = Just . take 10 . (`drop` chocolatePractice)
    }

substrLoc :: [Int] -> [Int] -> Int
substrLoc xs = length
             . takeWhile (not . (xs `isPrefixOf`))
             . tails

day14b :: [Int] :~> Int
day14b = MkSol
    { sParse = Just . mapMaybe (readMaybe . (:[]))
    , sShow  = show
    , sSolve = Just . (`substrLoc` chocolatePractice)
    }
