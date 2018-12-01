-- |
-- Module      : AOC2018.Interactive
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Versions of loaders and runners meant to be used in GHCI.
--

module AOC2018.Interactive (
    execChallenge
  , execChallengeWith
  , testChallenge
  , viewPrompt
  ) where

import           AOC2018
import           Control.Monad
import           Text.Printf
import qualified Data.Map      as M

-- | Run the solution indicated by the challenge spec on the official
-- puzzle input.
execChallenge :: ChallengeSpec -> IO ()
execChallenge cs@CS{..} = do
    Cfg{..} <- configFile "aoc-conf.yaml"
    c       <- case M.lookup _csPart <=< M.lookup _csDay $ challengeMap of
      Nothing -> fail "Challenge not yet implemented."
      Just c  -> pure c
    CD{..} <- challengeData _cfgSession cs
    case _cdInput of
      Right inp -> case runChallenge c inp of
        Right res -> putStrLn res
        Left  e   -> print e
      Left  e   -> print e

-- | Run the solution indicated by the challenge spec on a custom input.
execChallengeWith
    :: ChallengeSpec
    -> String               -- ^ custom puzzle input
    -> IO ()
execChallengeWith CS{..} inp = do
    Cfg{..} <- configFile "aoc-conf.yaml"
    c       <- case M.lookup _csPart <=< M.lookup _csDay $ challengeMap of
      Nothing -> fail "Challenge not yet implemented."
      Just c  -> pure c
    case runChallenge c inp of
      Right res -> putStrLn res
      Left  e   -> print e

-- | Run test suite for a given challenge spec.
testChallenge :: ChallengeSpec -> IO ()
testChallenge cs@CS{..} = do
    Cfg{..} <- configFile "aoc-conf.yaml"
    c       <- case M.lookup _csPart <=< M.lookup _csDay $ challengeMap of
      Nothing -> fail "Challenge not implemented."
      Just c  -> pure c
    CD{..} <- challengeData _cfgSession cs
    forM_ (zip [1..] _cdTests) $ \(i :: Int, (inp, ans)) ->
      case ans of
        Just (strip->ex) -> case runChallenge c inp of
          Right (strip->res)
            | ex == res -> printf "Test %d passed.\n" i
            | otherwise -> printf "Test %d failed.\n<<< %s\n>>> %s\n" i ex res
          Left e -> printf "Test %d failed.\n%s\n" i (show e)
        Nothing -> printf "Test %d skipped.\n" i

-- | View the prompt for a given challenge spec.
viewPrompt :: ChallengeSpec -> IO ()
viewPrompt cs@CS{..} = do
    Cfg{..} <- configFile "aoc-conf.yaml"
    CD{..} <- challengeData _cfgSession cs
    case _cdPrompt of
      Left  e -> print e
      Right p -> putStrLn p >> putStrLn ""

