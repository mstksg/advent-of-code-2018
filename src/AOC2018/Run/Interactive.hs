-- |
-- Module      : AOC2018.Run.Interactive
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Versions of loaders and runners meant to be used in GHCI.
--

module AOC2018.Run.Interactive (
    execSolution
  , execSolutionWith
  , testSolution
  , viewPrompt
  , submitSolution
  , loadInput
  , loadTests
  , mkSpec
  ) where

import           AOC2018.API
import           AOC2018.Challenge
import           AOC2018.Run
import           AOC2018.Run.Config
import           AOC2018.Run.Load
import           Control.Lens
import           Control.Monad.Except
import           Data.Finite
import           Data.Text            (Text)
import           Text.Printf
import qualified Data.Map             as M

-- | Run the solution indicated by the challenge spec on the official
-- puzzle input.
execSolution :: ChallengeSpec -> IO ()
execSolution cs = do
    cfg <- configFile "aoc-conf.yaml"
    out <- runExceptT . mainRun cfg . defaultMRO $ TSDayPart cs
    traverseOf_ (_Left . folded) putStrLn out

-- | Run the solution indicated by the challenge spec on a custom input.
execSolutionWith
    :: ChallengeSpec
    -> String               -- ^ custom puzzle input
    -> IO ()
execSolutionWith cs inp = do
    cfg <- configFile "aoc-conf.yaml"
    out <- runExceptT . mainRun cfg $ (defaultMRO (TSDayPart cs))
      { _mroInput = M.singleton (_csDay cs) . M.singleton (_csPart cs) $ inp
      }
    traverseOf_ (_Left . folded) putStrLn out

-- | Run test suite for a given challenge spec.
testSolution :: ChallengeSpec -> IO ()
testSolution cs = do
    cfg <- configFile "aoc-conf.yaml"
    out <- runExceptT . mainRun cfg $ (defaultMRO (TSDayPart cs))
      { _mroTest  = True
      }
    traverseOf_ (_Left . folded) putStrLn out

-- | View the prompt for a given challenge spec.
viewPrompt :: ChallengeSpec -> IO ()
viewPrompt cs@CS{..} = do
    cfg <- configFile "aoc-conf.yaml"
    out <- runExceptT $ mainView cfg MVO
      { _mvoSpec = cs
      }
    traverseOf_ (_Left . folded) putStrLn out

-- | Submit solution for a given challenge spec, and lock if correct.
submitSolution :: ChallengeSpec -> IO (Either [String] (Text, SubmitRes))
submitSolution cs = do
    cfg <- configFile "aoc-conf.yaml"
    runExceptT . mainSubmit cfg . defaultMSO $ cs

-- | Load input for a given challenge
loadInput :: ChallengeSpec -> IO (Either [String] String)
loadInput cs = do
    Cfg{..} <- configFile "aoc-conf.yaml"
    _cdInput <$> challengeData _cfgSession cs

-- | Load test cases for a given challenge
loadTests :: ChallengeSpec -> IO [(String, Maybe String)]
loadTests cs = do
    Cfg{..} <- configFile "aoc-conf.yaml"
    _cdTests <$> challengeData _cfgSession cs

-- | Unsafely create a 'ChallengeSpec' from a day number and part.
--
-- Is undefined if given a day number out of range (1-25).
mkSpec :: Integer -> Char -> ChallengeSpec
mkSpec i c = maybe e (`CS` c) . packFinite $ i - 1
  where
    e = errorWithoutStackTrace $ printf "Day out of range: %d" i

