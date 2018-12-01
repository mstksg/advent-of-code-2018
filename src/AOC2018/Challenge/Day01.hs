-- |
-- Module      : AOC2018.Challenge.Day1
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1!
--

module AOC2018.Challenge.Day01 (day01a, day01b) where

import           AOC2018.Prelude
import qualified Data.Set        as S

parseItem :: String -> Maybe Int
parseItem s = (readMaybe =<< ("+" `stripPrefix` s))
          <|> readMaybe s

firstRepeated :: Ord a => [a] -> Maybe a
firstRepeated = go S.empty
  where
    go s = \case
      x:xs
        | x `S.member` s -> Just x
        | otherwise      -> go (x `S.insert` s) xs
      []                 -> Nothing

day01a :: Challenge
day01a = MkC
    { cParse = traverse parseItem . lines
    , cShow  = show
    , cSolve = Just . sum
    }

day01b :: Challenge
day01b = MkC
    { cParse = traverse parseItem . lines
    , cShow  = show
    , cSolve = firstRepeated . scanl (+) 0 . cycle
    }
