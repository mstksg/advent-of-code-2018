{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC2018.Load
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Loading challenge data and prompts.
--

module AOC2018.Load (
    ChallengePaths(..), challengePaths
  , ChallengeData(..), challengeData
  ) where

import           AOC2018.API
import           AOC2018.Challenge
import           AOC2018.Util
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Finite
import           Data.Foldable
import           Data.List
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           Text.Printf
import qualified Data.Map                   as M
import qualified Data.Text                  as T

-- | A record of paths corresponding to a specific challenge.
data ChallengePaths = CP { _cpPrompt    :: !FilePath
                         , _cpInput     :: !FilePath
                         , _cpAnswer    :: !FilePath
                         , _cpTests     :: !FilePath
                         }
  deriving Show

-- | A record of data (test inputs, answers) corresponding to a specific
-- challenge.
data ChallengeData = CD { _cdPrompt :: !(Either [String] String)
                        , _cdInput  :: !(Either [String] String)
                        , _cdAnswer :: !(Maybe String)
                        , _cdTests  :: ![(String, Maybe String)]
                        }

-- | Generate a 'ChallengePaths' from a specification of a challenge.
challengePaths :: ChallengeSpec -> ChallengePaths
challengePaths (CS d p) = CP
    { _cpPrompt    = "prompt"    </> printf "%02d%c" d' p <.> "txt"
    , _cpInput     = "data"      </> printf "%02d" d'     <.> "txt"
    , _cpAnswer    = "data/ans"  </> printf "%02d%c" d' p <.> "txt"
    , _cpTests     = "test-data" </> printf "%02d%c" d' p <.> "txt"
    }
  where
    d' = getFinite d + 1

makeChallengeDirs :: ChallengePaths -> IO ()
makeChallengeDirs CP{..} =
    mapM_ (createDirectoryIfMissing True . takeDirectory)
          [_cpPrompt, _cpInput, _cpAnswer, _cpTests]

-- | Load data associated with a challenge from a given specification.
-- Will fetch answers online and cache if required (and if giten a session
-- token).
challengeData
    :: Maybe String
    -> ChallengeSpec
    -> IO ChallengeData
challengeData sess spec = do
    makeChallengeDirs ps
    inp   <- runExceptT . asum $
      [ ExceptT $ maybe (Left [fileErr]) Right <$> readFileMaybe _cpInput
      , fetchInput
      ]
    prompt <- runExceptT . asum $
      [ ExceptT $ maybe (Left [fileErr]) Right <$> readFileMaybe _cpPrompt
      , fetchPrompt
      ]
    ans    <- readFileMaybe _cpAnswer
    ts     <- foldMap (parseTests . lines) <$> readFileMaybe _cpTests
    return CD
      { _cdPrompt = prompt
      , _cdInput  = inp
      , _cdAnswer = ans
      , _cdTests  = ts
      }
  where
    ps@CP{..} = challengePaths spec
    fileErr = printf "Input file not found at %s" _cpInput
    readFileMaybe :: FilePath -> IO (Maybe String)
    readFileMaybe =
        (traverse (evaluate . force) . either (const Nothing) Just =<<)
       . tryJust (guard . isDoesNotExistError)
       . readFile
    fetchInput :: ExceptT [String] IO String
    fetchInput = do
        s <- maybeToEither ["Session key needed to fetch input"] $
              sessionKey a sess
        fmap T.unpack . ExceptT $ runAPI s a
      where
        a = AInput $ _csDay spec
    fetchPrompt :: ExceptT [String] IO String
    fetchPrompt = do
        prompts <- ExceptT $ runAPI (sessionKey_ sess) a
        fmap T.unpack
          . maybeToEither [e]
          . M.lookup (_csPart spec)
          $ prompts
      where
        a = APrompt $ _csDay spec
        e = case sess of
          Just _  -> "Part not yet released"
          Nothing -> "Part not yet released, or may require session key"
    parseTests :: [String] -> [(String, Maybe String)]
    parseTests xs = case break (">>> " `isPrefixOf`) xs of
      (inp,[])
        | null (strip (unlines inp))  -> []
        | otherwise -> [(unlines inp, Nothing)]
      (inp,(strip.drop 4->ans):rest)
        | null (strip (unlines inp))  -> parseTests rest
        | otherwise ->
            let ans' = ans <$ guard (not (null ans))
            in  (unlines inp, ans') : parseTests rest

