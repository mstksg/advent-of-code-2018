-- |
-- Module      : AOC.Challenge.Day09
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day09 (
    day09a
  , day09b
  ) where

import           AOC.Solver                 ((:~>)(..))
import           Control.Lens                   (ix, (+~))
import           Data.Bifunctor                 (second)
import           Data.Function                  ((&))
import           Data.List                      (foldl')
import           Data.List.PointedList.Circular (PointedList(..))
import           Data.Maybe                     (mapMaybe, fromJust)
import           Text.Read                      (readMaybe)
import qualified Data.List.PointedList.Circular as PL
import qualified Data.Vector.Unboxed            as V

place
    :: Int                       -- ^ number to place
    -> PointedList Int           -- ^ tape
    -> (Int, PointedList Int)    -- ^ resulting tape, and scored points
place x l
    | x `mod` 23 == 0
    = let l'       = PL.moveN (-7) l
          toAdd    = _focus l'
      in  (toAdd + x, fromJust (PL.deleteRight l'))
    | otherwise
    = (0, (PL.insertLeft x . PL.moveN 2) l)

run
    :: Int                  -- ^ number of players
    -> Int                  -- ^ Max # of piece
    -> V.Vector Int
run numPlayers maxPiece = fst
                        . foldl' go (V.replicate numPlayers 0, PL.singleton 0)
                        $ zip players toInsert
  where
    go (!scores, !tp) (!p, !i) = (scores & ix p +~ pts, tp')
      where
        (pts, tp') = place i tp
    players  = (`mod` numPlayers) <$> [0 ..]
    toInsert = [1..maxPiece]

day09a :: (Int, Int) :~> Int
day09a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

day09b :: _ :~> _
day09b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

parse :: String -> Maybe (Int, Int)
parse xs = case mapMaybe readMaybe (words xs) of
    [p, n] -> Just (p, n)
    _      -> Nothing
