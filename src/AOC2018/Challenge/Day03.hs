-- |
-- Module      : AOC2018.Challenge.Day03
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC2018.Solver" for the types used in this module!

module AOC2018.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC2018.Solver ((:~>)(..))
import           AOC2018.Util   (freqs)
import           Control.Monad  (guard)
import           Data.Char      (isDigit)
import           Data.Ix        (range)
import           Data.Map       (Map)
import           Data.Maybe     (mapMaybe, listToMaybe)
import qualified Data.Map       as M

type Coord = (Int, Int)

data Rect = R { _rStart :: Coord
              , _rSize  :: Coord
              }

parseLine :: String -> Maybe (Int, Rect)
parseLine = mkLine
          . map read
          . words
          . map onlyDigits
  where
    mkLine [i,x0,y0,w,h] = Just (i, (R (x0,y0) (w, h)))
    mkLine _             = Nothing
    onlyDigits c
      | isDigit c = c
      | otherwise = ' '

tiles :: Rect -> [Coord]
tiles (R (x0, y0) (w, h)) = range ((x0, y0), (x0 + w - 1, y0 + h - 1))

mkMap :: [Rect] -> Map Coord Int
mkMap = freqs . concatMap tiles

day03a :: [(Int, Rect)] :~> Int
day03a = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = Just . length . filter (>= 2) . M.elems . mkMap . map snd
    }

day03b :: _ :~> Int
day03b = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = \ts -> let mp = mkMap (snd <$> ts)
                      in  listToMaybe . mapMaybe (noOverlap mp) $ ts
    }

noOverlap :: Map Coord Int -> (Int, Rect) -> Maybe Int
noOverlap mp (i, r) = i <$ guard (all isAlone (tiles r))
  where
    isAlone c = M.lookup c mp == Just 1
