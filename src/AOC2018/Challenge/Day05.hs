-- |
-- Module      : AOC2018.Challenge.Day05
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC2018.Solver" for the types used in this module!

module AOC2018.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC2018.Solver ((:~>)(..))
import           Data.Char      (toLower)

anti :: Char -> Char -> Bool
anti x y = toLower x == toLower y && x /= y

cons :: Char -> String -> String
x `cons` (y:xs)
    | anti x y  = xs
    | otherwise = x:y:xs
x `cons` []     = [x]

react :: String -> String
react = foldr cons []

day05a :: String :~> Int
day05a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . length . react
    }

day05b :: String :~> Int
day05b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = \xs -> Just $ minimum [ length $ react (remove c xs)
                                     | c <- ['a' .. 'z']
                                     ]
    }
  where
    remove c = filter $ (/= c) . toLower
