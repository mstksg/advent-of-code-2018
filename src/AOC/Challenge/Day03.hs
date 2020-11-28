-- |
-- Module      : AOC.Challenge.Day03
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Common    (freqs, findMaybe, clearOut)
import           AOC.Solver    ((:~>)(..))
import           Control.Monad (guard)
import           Data.Char     (isDigit)
import           Data.Ix       (range)
import           Data.Map      (Map)
import           Data.Maybe    (mapMaybe)
import           Linear        (V2(..))
import           Text.Read     (readMaybe)
import qualified Data.Map      as M

-- | x and y
type Coord = V2 Int

-- | Claim ID and rectangle
data Claim = C { _cId   :: Int
               , _cRect :: Rect
               }

-- | Start <x,y>, and size <w,h>
data Rect = R { _rStart :: Coord
              , _rSize  :: Coord
              }

-- | Attempt to parse a line into @(Int, Rect)@ (a claim ID #, and the
-- rectangle claimed)
parseLine :: String -> Maybe Claim
parseLine = mkLine
          . mapMaybe readMaybe
          . words
          . clearOut (not . isDigit)
  where
    mkLine [i,x0,y0,w,h] = Just $ C i (R (V2 x0 y0) (V2 w h))
    mkLine _             = Nothing

-- | Get a list of all coordinates within a given rectangle specification
tiles :: Rect -> [Coord]
tiles (R start size) = range (start, start + size - 1)

-- | Generate a frequency map of tiles to number of claims at that tile
layTiles :: [Rect] -> Map Coord Int
layTiles = freqs . concatMap tiles

-- | Lends itself pretty well to a functional approach.
--
-- 1. Lay the tiles.
-- 2. Get all the frequencies at each time
-- 3. Filter for the frequencies greater than 2
-- 4. Count them
day03a :: [Rect] :~> Int
day03a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
             . length           -- > how many?
             . filter (>= 2)    -- > only frequencies >= 2
             . M.elems          -- > get all frequencies
             . layTiles         -- > lay the tiles
    }

-- | Once we lay our tiles, we find the first claim that has no overlaps.
day03b :: [Claim] :~> Int
day03b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

-- | Given a map of tiles claimed (and how many are claiming that spot) and
-- a claim ID and rectangle, check if all of the claim's rectangles are
-- alone in the map (are claimed by only one claim).
--
-- If yes, return the ID of the claim.
noOverlap
    :: Map Coord Int
    -> Claim
    -> Maybe Int
noOverlap tilesClaimed C{..} = _cId <$ guard (all isAlone (tiles _cRect))
  where
    isAlone c = M.lookup c tilesClaimed == Just 1
