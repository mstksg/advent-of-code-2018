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
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC2018.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC2018.Challenge.Day09 (
    day09a
  , day09b
  ) where

import           AOC2018.Prelude
import           Control.Lens
import           Data.List.PointedList.Circular (PointedList(..))
import qualified Data.List.PointedList.Circular as PL
import qualified Data.Vector.Unboxed            as V

place
    :: Int                       -- ^ number to place
    -> PointedList Int           -- ^ tape
    -> (PointedList Int, Int)    -- ^ resulting tape, and scored points
place x l
    | x `mod` 23 == 0
    =       let l'       = PL.moveN (-7) l
                toAdd    = _focus l'
                Just l'' = PL.deleteRight l'
            in  (l'', toAdd + x)
    | otherwise
    = ((PL.insertLeft x . PL.moveN 2) l, 0)

run :: Int -> Int -> V.Vector Int
run numPlayers numList = fst $ foldl' go (v0, PL.singleton 0) (zip players toInsert)
  where
    v0 = V.replicate numPlayers 0
    go (scores, tp) (p, i) = case place i tp of
                               (tp', pts) -> (scores & ix p +~ pts, tp')
    players  = (`mod` numPlayers) <$> [0 ..]
    toInsert = [1..numList]

day09a :: (Int, Int) :~> Int
day09a = MkSol
    { sParse = (\[p,n] -> Just (p, n)) . mapMaybe readMaybe . words
    , sShow  = show
    , sSolve = \(p, n) -> Just . V.maximum $ run p n
    }

day09b :: _ :~> _
day09b = MkSol
    { sParse = (\[p,n] -> Just (p, n)) . mapMaybe readMaybe . words
    , sShow  = show
    , sSolve = \(p, n) -> Just . V.maximum $ run p (n * 100)
    }
