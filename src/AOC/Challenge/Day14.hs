module AOC.Challenge.Day14 (
    day14a
  , day14b
  ) where

import           AOC.Solver    ((:~>)(..))
import           Data.List     (tails, isPrefixOf)
import           Data.Maybe    (mapMaybe)
import           Data.Sequence (Seq(..))
import           Text.Read     (readMaybe)
import qualified Data.Sequence as Seq

data Tape = T { _tp1    :: !Int
              , _tp2    :: !Int
              , _tSeq   :: !(Seq Int)
              }
  deriving Show

step :: Tape -> ([Int], Tape)      -- tape, and "new digits"
step T{..} = (newDigits, T newTp1 newTp2 newSeq)
  where
    sc1 = _tSeq `Seq.index` _tp1
    sc2 = _tSeq `Seq.index` _tp2
    newDigits = digitize $ sc1 + sc2
    newSeq = _tSeq Seq.>< Seq.fromList newDigits
    newTp1 = (_tp1 + sc1 + 1) `mod` length newSeq
    newTp2 = (_tp2 + sc2 + 1) `mod` length newSeq

digitize :: Int -> [Int]
digitize ((`divMod` 10)->(x,y))
    | x == 0    = [y]
    | otherwise = [x,y]

chocolatePractice :: [Int]
chocolatePractice = 3 : 7 : go (T 0 1 (Seq.fromList [3,7]))
  where
    go (step->(out,t)) = out ++ go t

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
