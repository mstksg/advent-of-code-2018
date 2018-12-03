-- |
-- Module      : AOC2018.Challenge.Day01
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1!  See "AOC2018.Solver" for the types used in this module!

module AOC2018.Challenge.Day01 (day01a, day01b) where

import           AOC2018.Solver ((:~>)(..))
import           AOC2018.Util   (firstRepeated)
import           Text.Read      (readMaybe)

-- | We need this because 'read' can't handle positive signs in front of
-- numbers.
parseItem :: String -> Maybe Int
parseItem = readMaybe . filter (/= '+')

-- | Here we have a basic sum of numbers.
day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = traverse parseItem . lines
    , sShow  = show
    , sSolve = Just . sum
    }

-- | Here we compute a running sum on an infinitely repeated list of
-- inputs, and then use 'firstRepeated' to get the first repeated item in
-- the list of running sums.
day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = traverse parseItem . lines
    , sShow  = show
    , sSolve = firstRepeated        -- > get first repeated sum
             . scanl (+) 0          -- > compute running sum
             . cycle                -- > infinitely cycle input
    }
