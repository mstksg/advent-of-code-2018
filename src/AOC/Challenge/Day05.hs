-- |
-- Module      : AOC.Challenge.Day05
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
--
-- Note that this is slow in the current version of "Data.Group.Free" on
-- hackage.  See https://github.com/mstksg/free-algebras/tree/freegroup2
-- for a version that is efficient.
--
-- See <https://blog.jle.im/entry/alchemical-groups.html> for an
-- explaination.

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Common        (deleteFinite)
import           AOC.Solver        ((:~>)(..))
import           Data.Algebra.Free (returnFree, foldMapFree)
import           Data.Char         (ord, toLower, isLower)
import           Data.Finite       (Finite, packFinite, finites)
import           Data.Group        (invert)
import           Data.Group.Free   (FreeGroupL)
import qualified Data.Group.Free   as G

-- | One of the generators from the full alphabet
type Elem  = Finite 26

charElem
    :: Char
    -> Maybe (Either Elem Elem)     -- left if lower, right if upper
charElem c
    | isLower c = Left <$> i
    | otherwise = Right <$> i
  where
    i = packFinite (fromIntegral (ord (toLower c) - ord 'a'))

inject
    :: Char
    -> FreeGroupL Elem
inject = foldMap (either returnFree (invert . returnFree)) . charElem

day05a :: FreeGroupL Elem :~> Int
day05a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

day05b :: FreeGroupL Elem :~> Int
day05b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
  where
    -- | Delete a letter from the group
    ghomo :: Elem -> Elem -> FreeGroupL (Finite 25)
    ghomo c = foldMap returnFree . deleteFinite c

-- -------------
-- | Old Methods
-- -------------

-- anti :: Char -> Char -> Bool
-- anti x y = toLower x == toLower y && x /= y

-- cons :: Char -> String -> String
-- x `cons` (y:xs)
--     | anti x y  = xs
--     | otherwise = x:y:xs
-- x `cons` []     = [x]

-- day05a :: String :~> Int
-- day05a = MkSol
--     { sParse = Just
--     , sShow  = show
--     , sSolve = Just . length . foldr cons []
--     }

-- day05b :: String :~> Int
-- day05b = MkSol
--     { sParse = Just
--     , sShow  = show
--     , sSolve = \xs -> Just $ minimum [ length $ foldr cons [[] (remove c xs)
--                                      | c <- ['a' .. 'z']
--                                      ]
--     }
--   where
--     remove c = filter $ (/= c) . toLower
