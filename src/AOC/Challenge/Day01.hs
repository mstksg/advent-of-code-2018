-- |
-- Module      : AOC.Challenge.Day01
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1!  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day01 (day01a, day01b) where

import           AOC.Common (firstRepeated)
import           AOC.Solver ((:~>)(..))
import           Text.Read  (readMaybe)

-- | We need this because 'read' can't handle positive signs in front of
-- numbers.
parseItem :: String -> Maybe Int
parseItem = readMaybe . filter (/= '+')

-- | Here we have a basic sum of numbers.
day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

-- | Here we compute a running sum on an infinitely repeated list of
-- inputs, and then use 'firstRepeated' to get the first repeated item in
-- the list of running sums.
day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
