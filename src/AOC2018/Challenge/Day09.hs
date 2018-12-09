-- |
-- Module      : AOC2018.Challenge.Day09
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC2018.Solver" for the types used in this module!

module AOC2018.Challenge.Day09 (
    day09a
  , day09b
  ) where

import           AOC2018.Solver                 ((:~>)(..))
import           Control.Lens                   (ix, (+~))
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
    =   let l'       = PL.moveN (-7) l
            toAdd    = _focus l'
        in  (toAdd + x, fromJust (PL.deleteRight l'))
    | otherwise
    = (0, (PL.insertLeft x . PL.moveN 2) l)

run :: Int -> Int -> V.Vector Int
run numPlayers numList = fst
                       . foldl' go (v0, PL.singleton 0)
                       $ zip players toInsert
  where
    v0 = V.replicate numPlayers 0
    go (!scores, !tp) (!p, !i) = (scores & ix p +~ pts, tp')
      where
        (pts, tp') = place i tp
    players  = (`mod` numPlayers) <$> [0 ..]
    toInsert = [1..numList]

day09a :: (Int, Int) :~> Int
day09a = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just . V.maximum . uncurry run 
    }

day09b :: _ :~> _
day09b = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just . V.maximum . uncurry run
    }

parse :: String -> Maybe (Int, Int)
parse xs = case mapMaybe readMaybe (words xs) of
    [p, n] -> Just (p, n)
    _      -> Nothing
