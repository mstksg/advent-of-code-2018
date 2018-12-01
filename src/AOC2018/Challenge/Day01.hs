-- |
-- Module      : AOC2018.Challenge.Day1
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1!  Pretty straightforward Haskell pipeline.  Parse using
-- 'readMaybe' (remembering to 'stripPrefix' a positive sign first).
--
-- The first one is just a simple sum.
--
-- The second one lazily computes a running sum with @'scanl' (+) 0@ on the
-- infinitely repeated list of inputs ('cycle'), and then uses a custom
-- utility function 'firstRepeated' to get the first repeated running sum.
--

module AOC2018.Challenge.Day01 (day01a, day01b) where

import           AOC2018.Types       (Challenge(..))
import           AOC2018.Util        (firstRepeated)
import           Control.Applicative ((<|>))
import           Data.List           (stripPrefix)
import           Text.Read           (readMaybe)

parseItem :: String -> Maybe Int
parseItem s = (readMaybe =<< ("+" `stripPrefix` s))
          <|> readMaybe s

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
