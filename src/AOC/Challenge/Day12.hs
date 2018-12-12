{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day12
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day12 where
-- module AOC.Challenge.Day12 (
--     day12a
--   , day12b
--   ) where

import           AOC.Prelude
import           Control.Lens
import           Control.Monad.Fail
import           Control.Monad.ST
import           Data.Word
import qualified Data.IntMap         as IM
import qualified Data.IntSet         as IS
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

type Ctx = Set (Finite 5)

makeState :: String -> IntSet
makeState = IM.keysSet
          . IM.filter (== '#')
          . IM.fromList
          . zip [0..]
          . filter (`elem` "#.")

makeCtxs :: String -> Set Ctx
makeCtxs = M.keysSet
         . M.filter id
         . M.fromList
         . map ( bimap parseLine head
               . splitAt 5
               . map (== '#')
               . filter (`elem` "#.")
               )
         . lines
  where
    parseLine = S.fromList . map fst . filter snd . zip [0..]


step
    :: Set Ctx
    -> IntSet
    -> IntSet
step ctxs w0 = IS.fromDistinctAscList
             . filter go
             $ [IS.findMin w0 - 2 .. IS.findMax w0 + 2]
  where
    go i = neighbs `S.member` ctxs
      where
        neighbs = flip S.filter (S.fromList [0 .. 4]) $ \j ->
          (i - 2 + fromIntegral j) `IS.member` w0

findLoop
    :: Set Ctx
    -> IntSet
    -> (Int, Int, Int)      -- time to loop, loop size, loop incr
findLoop ctxs w0 = go (M.singleton w0 (0, 0)) 1 w0
  where
    go !seen !i !w = case M.lookup w'Norm seen of
        Nothing              -> go (M.insert w'Norm (mn, i) seen) (i + 1) w'
        Just (seenMn, seenI) -> (seenI, i - seenI, mn - seenMn)
      where
        w'           = step ctxs w
        (mn, w'Norm) = normalize w'
    normalize w = (mn, IS.map (subtract mn) w)
      where
        mn = IS.findMin w

stepN
    :: Set Ctx
    -> Int
    -> IntSet
    -> IntSet
stepN ctx n mp = goN extra
               . IS.map (+ (loopIncr * looped))
               . goN ttl
               $ mp
  where
    goN m = (!!! m) . iterate (step ctx)
    (ttl, loopSize, loopIncr) = findLoop ctx mp
    (looped, extra) = (n - ttl) `divMod` loopSize

countPlants :: IntSet -> Int
countPlants = sum . IS.toList

day12a :: (IntSet, Set Ctx) :~> Int
day12a = MkSol
    { sParse = Just . bimap makeState makeCtxs . span (/= '\n')
    , sShow  = show
    , sSolve = \(mp, ctx) -> Just . countPlants $ iterate (step ctx) mp !!! 20
    }

day12b :: (IntSet, Set Ctx) :~> Int
day12b = MkSol
    { sParse = Just . bimap makeState makeCtxs . span (/= '\n')
    , sShow  = show
    , sSolve = \(mp, ctx) -> Just . countPlants $ stepN ctx 50000000000 mp
    }
