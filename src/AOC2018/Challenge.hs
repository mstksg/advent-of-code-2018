{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : AOC2018.Challenge
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Gather together all challenges and collect them into a single map.
--

module AOC2018.Challenge (
    module AOC
  , ChallengeMap
  , ChallengeSpec(..), Part(..)
  , challengeMap
  , lookupSolution
  , dayToInt
  , solSpec
  , charPart
  ) where

import           AOC2018.Challenge.Day01 as AOC
import           AOC2018.Challenge.Day02 as AOC
import           AOC2018.Challenge.Day03 as AOC
import           AOC2018.Challenge.Day04 as AOC
import           AOC2018.Challenge.Day05 as AOC
import           AOC2018.Challenge.Day06 as AOC
import           AOC2018.Challenge.Day07 as AOC
import           AOC2018.Challenge.Day08 as AOC
import           AOC2018.Challenge.Day09 as AOC
import           AOC2018.Challenge.Day10 as AOC
import           AOC2018.Challenge.Day11 as AOC
import           AOC2018.Challenge.Day12 as AOC
import           AOC2018.Challenge.Day13 as AOC
import           AOC2018.Challenge.Day14 as AOC
import           AOC2018.Challenge.Day15 as AOC
import           AOC2018.Challenge.Day16 as AOC
import           AOC2018.Challenge.Day17 as AOC
import           AOC2018.Challenge.Day18 as AOC
import           AOC2018.Challenge.Day19 as AOC
import           AOC2018.Challenge.Day20 as AOC
import           AOC2018.Challenge.Day21 as AOC
import           AOC2018.Challenge.Day22 as AOC
import           AOC2018.Challenge.Day23 as AOC
import           AOC2018.Challenge.Day24 as AOC
import           AOC2018.Challenge.Day25 as AOC

import           AOC2018.Discover
import           AOC2018.Solver
import           Advent
import           Control.Monad
import           Data.Finite
import           Data.Map         (Map)
import qualified Data.Map         as M

-- | A map of all challenges.
challengeMap :: ChallengeMap
challengeMap = mkChallengeMap $$(solutionList "src/AOC2018/Challenge")

-- | Lookup up a solution from a 'ChallengeMap'
lookupSolution :: ChallengeSpec -> Map (Finite 25) (Map Part a) -> Maybe a
lookupSolution CS{..} = M.lookup _csPart <=< M.lookup _csDay
