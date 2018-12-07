{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2018.Challenge.Day07
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC2018.Solver" for the types used in this module!
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

module AOC2018.Challenge.Day07 (
    day07a
  , day07b
  ) where

import           AOC2018.Prelude
import           Control.Lens
import           Control.Monad.State
import qualified Data.Graph.Inductive as G
import qualified Data.Map             as M
import qualified Data.Set             as S

parseLine :: String -> (Char, Char)
parseLine (filter isUpper->(_:a:b:_)) = (a, b)

flipMap :: (Ord a, Ord b) => Map a (Set b) -> Map b (Set a)
flipMap = M.fromListWith (<>)
        . map (second S.singleton . swap)
        . concatMap (traverse toList)
        . M.toList

findRoots :: Map Char (Set Char) -> Set Char
findRoots mp = cs `S.difference` targs
  where
    cs = M.keysSet mp
    targs = S.unions $ toList mp

build :: Map Char (Set Char) -> Set Char -> [Char]
build mp = go S.empty
  where
    deps = flipMap mp
    go :: Set Char -> Set Char -> [Char]
    go done active = case find isReady active of
      Just c -> let done'   = S.insert c done
                    active' = S.delete c active <> fold (M.lookup c mp)
                in  c : go done' active'
      Nothing -> []
      where
        isReady = S.null . (`S.difference` done) . fold . (`M.lookup` deps)

day07a :: Map Char (Set Char) :~> String
day07a = MkSol
    { sParse = Just . M.fromListWith (<>) . map (second S.singleton . parseLine) . lines
    , sShow  = id
    , sSolve = \mp ->
        let rts = findRoots mp
        in  Just $ build mp rts
    }

waitTime :: Char -> Int
waitTime = (+ 61) . subtract (ord 'A') . ord

tick :: Int -> Either () Int
tick i
    | i <= 1    = Left ()
    | otherwise = Right (i - 1)

build2 :: Map Char (Set Char) -> Set Char -> ([(Set Char, Map Char Int, Set Char)], Int)
build2 mp xs0 = go S.empty (M.fromSet waitTime xs1) xs2
  where
    (xs1,xs2) = S.splitAt 5 xs0
    deps = flipMap mp
    go :: Set Char -> Map Char Int -> Set Char -> ([(Set Char, Map Char Int, Set Char)], Int)
    go done active waiting
        | M.null active'' = ([(done,active,waiting)],1)
        | otherwise       = (+) <$> ([(done,active,waiting)],1) <*> go done' active'' (waiting'' <> ineligible)
      where
        (M.keysSet->expired,active') = M.mapEither tick active
        done'    = done <> expired
        newWaiting = foldMap (fold . (`M.lookup` mp)) expired
        waiting'  = waiting <> newWaiting
        toAdd     = 5 - M.size active'
        (eligible, ineligible) = S.partition isReady waiting'
        (adding, waiting'') = S.splitAt toAdd eligible
        active'' = active' <> M.fromSet waitTime adding

        isReady = S.null . (`S.difference` done') . fold . (`M.lookup` deps)

day07b :: Map Char (Set Char) :~> Int
day07b = MkSol
    { sParse = Just . M.fromListWith (<>) . map (second S.singleton . parseLine) . lines
    , sShow  = show
    , sSolve = \mp ->
        let rts = findRoots mp
        in  Just . snd $ build2 mp rts
    }
