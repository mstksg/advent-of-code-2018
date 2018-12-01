{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : AOC2018
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Gather together all challenges and collect them into a single map.  Also
-- re-exports general utilities used by the auto-runner executable.
--

module AOC2018 (
    module AOC
  , challengeMap
  ) where

import           AOC2018.Challenge.Day01 as AOC
import           AOC2018.Challenge.Day02 as AOC
-- import           AOC2018.Challenge.Day03 as AOC
-- import           AOC2018.Challenge.Day04 as AOC
-- import           AOC2018.Challenge.Day05 as AOC
-- import           AOC2018.Challenge.Day06 as AOC
-- import           AOC2018.Challenge.Day07 as AOC
-- import           AOC2018.Challenge.Day08 as AOC
-- import           AOC2018.Challenge.Day09 as AOC
-- import           AOC2018.Challenge.Day10 as AOC
-- import           AOC2018.Challenge.Day11 as AOC
-- import           AOC2018.Challenge.Day12 as AOC
-- import           AOC2018.Challenge.Day13 as AOC
-- import           AOC2018.Challenge.Day14 as AOC
-- import           AOC2018.Challenge.Day15 as AOC
-- import           AOC2018.Challenge.Day16 as AOC
-- import           AOC2018.Challenge.Day17 as AOC
-- import           AOC2018.Challenge.Day18 as AOC
-- import           AOC2018.Challenge.Day19 as AOC
-- import           AOC2018.Challenge.Day20 as AOC
-- import           AOC2018.Challenge.Day21 as AOC
-- import           AOC2018.Challenge.Day22 as AOC
-- import           AOC2018.Challenge.Day23 as AOC
-- import           AOC2018.Challenge.Day24 as AOC
-- import           AOC2018.Challenge.Day25 as AOC

import           AOC2018.Challenge as AOC
import           AOC2018.Config    as AOC
import           AOC2018.Discover
import           AOC2018.Load      as AOC
import           AOC2018.Util      as AOC

-- | A map of all challenges.
challengeMap :: SolutionMap
challengeMap = mkSolutionMap $$(solutionList "src/AOC2018/Challenge")
