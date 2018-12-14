{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day14
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day14 where

-- module AOC.Challenge.Day14 (
--     day14a
--   , day14b
--   ) where

import           AOC.Prelude
import           Data.Sequence (Seq(..))
import           Control.Lens
import qualified Data.Sequence as Seq

data Tape = T { _tp1 :: !Int, _tp2 :: !Int, _tSeq :: !(Seq Int) }
  deriving Show

makeLenses ''Tape

step :: Tape -> (Bool, Tape)     -- True if added two digits
step T{..} = (isJust add1, T newTp1 newTp2 newSeq)
  where
    sc1 = _tSeq `Seq.index` _tp1
    sc2 = _tSeq `Seq.index` _tp2
    (add1, add2) = digitize $ sc1 + sc2
    newSeq = _tSeq Seq.>< (foldMap Seq.singleton add1 Seq.|> add2)
    newTp1 = (_tp1 + sc1 + 1) `mod` length newSeq
    newTp2 = (_tp2 + sc2 + 1) `mod` length newSeq

digitize :: Int -> (Maybe Int, Int)
digitize = first (mfilter (> 0) . Just) . (`divMod` 10)

runUntil :: Int -> [Int]
runUntil lim = go (T 0 1 (Seq.fromList [3,7]))
  where
    go !t
        | length _tSeq > (lim + 9) = take 10 . toList . Seq.drop lim $ _tSeq
        | otherwise                = go t'
      where
        (_, t'@T{..}) = step t

day14a :: Int :~> [Int]
day14a = MkSol
    { sParse = readMaybe
    , sShow  = concatMap show
    , sSolve = Just . runUntil
    }

runUntil2 :: Seq Int -> Int
runUntil2 lim = go (T 0 1 (Seq.fromList [3,7]))
  where
    go t
        |              takeLast limSize       _tSeq `matchPrefix` lim = length _tSeq - limSize
        | checkBoth && takeLast (limSize + 1) _tSeq `matchPrefix` lim = length _tSeq - limSize - 1
        | otherwise             = go t'
      where
        (checkBoth, t'@T{..}) = step t
        lastItems1 = takeLast limSize _tSeq
        lastItems2 = dropLast 1 . takeLast (limSize + 1) $ _tSeq
    limSize = length lim

takeLast :: Int -> Seq a -> Seq a
takeLast i s = Seq.drop (length s - i) s

dropLast :: Int -> Seq a -> Seq a
dropLast i s = Seq.take (length s - i) s

matchPrefix :: Eq a => Seq a -> Seq a -> Bool
matchPrefix xs = and . Seq.zipWith (==) xs

day14b :: _ :~> _
day14b = MkSol
    { sParse = Just . Seq.fromList . mapMaybe (readMaybe . (:[]))
    , sShow  = show
    , sSolve = Just . runUntil2
    }
