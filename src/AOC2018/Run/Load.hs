{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC2018.Run.Load
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Loading challenge data and prompts.
--

module AOC2018.Run.Load (
    ChallengePaths(..), challengePaths
  , ChallengeData(..), challengeData
  , countdownConsole
  , timeToRelease
  , showNominalDiffTime
  ) where

import           AOC2018.API
import           AOC2018.Challenge
import           AOC2018.Util
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Data.Finite
import           Data.Foldable
import           Data.List
import           Data.Text            (Text)
import           Data.Time
import           System.Console.ANSI  as ANSI
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Error
import           Text.Printf
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

-- | A record of paths corresponding to a specific challenge.
data ChallengePaths = CP { _cpPrompt    :: !FilePath
                         , _cpInput     :: !FilePath
                         , _cpAnswer    :: !FilePath
                         , _cpTests     :: !FilePath
                         , _cpLog       :: !FilePath
                         }
  deriving Show

-- | A record of data (test inputs, answers) corresponding to a specific
-- challenge.
data ChallengeData = CD { _cdPrompt :: !(Either [String] Text  )
                        , _cdInput  :: !(Either [String] String)
                        , _cdAnswer :: !(Maybe String)
                        , _cdTests  :: ![(String, Maybe String)]
                        }

-- | Generate a 'ChallengePaths' from a specification of a challenge.
challengePaths :: ChallengeSpec -> ChallengePaths
challengePaths (CS d p) = CP
    { _cpPrompt    = "prompt"          </> printf "%02d%c" d' p <.> "txt"
    , _cpInput     = "data"            </> printf "%02d" d'     <.> "txt"
    , _cpAnswer    = "data/ans"        </> printf "%02d%c" d' p <.> "txt"
    , _cpTests     = "test-data"       </> printf "%02d%c" d' p <.> "txt"
    , _cpLog       = "logs/submission" </> printf "%02d%c" d' p <.> "txt"
    }
  where
    d' = dayToInt d

makeChallengeDirs :: ChallengePaths -> IO ()
makeChallengeDirs CP{..} =
    mapM_ (createDirectoryIfMissing True . takeDirectory)
          [_cpPrompt, _cpInput, _cpAnswer, _cpTests, _cpLog]

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
      [ maybeToEither [printf "Input file not found at %s" _cpInput]
          =<< liftIO (readFileMaybe _cpInput)
      , fetchInput
      ]
    prompt <- runExceptT . asum $
      [ maybeToEither [printf "Prompt file not found at %s" _cpPrompt]
          =<< liftIO (fmap T.pack <$> readFileMaybe _cpPrompt)
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
    readFileMaybe :: FilePath -> IO (Maybe String)
    readFileMaybe =
        (traverse (evaluate . force) . either (const Nothing) Just =<<)
       . tryJust (guard . isDoesNotExistError)
       . readFile
    fetchInput :: ExceptT [String] IO String
    fetchInput = do
        s <- maybeToEither ["Session key needed to fetch input"] $
              sessionKey a sess
        inp <- fmap T.unpack . ExceptT $ runAPI s a
        liftIO $ writeFile _cpInput inp
        pure inp
      where
        a = AInput $ _csDay spec
    fetchPrompt :: ExceptT [String] IO Text
    fetchPrompt = do
        prompts <- ExceptT $ runAPI (sessionKey_ sess) a
        prompt  <- maybeToEither [e]
                 . M.lookup (_csPart spec)
                 $ prompts
        liftIO $ T.writeFile _cpPrompt prompt
        pure prompt
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

-- | Get time until next release
timeToRelease :: Finite 25 -> IO NominalDiffTime
timeToRelease d = (challengeReleaseTime d `diffUTCTime`) <$> getCurrentTime

-- | Challenge release time
challengeReleaseTime :: Finite 25 -> UTCTime
challengeReleaseTime d = UTCTime (fromGregorian 2018 12 (dayToInt d))
                                 (5 * 60 * 60)

-- | Pretty-print a 'NominalDiffTime'
showNominalDiffTime :: NominalDiffTime -> String
showNominalDiffTime (round @Double @Int . realToFrac -> rawSecs) =
    printf "%02dd %02d:%02d:%02d" days hours mins secs
  where
    (rawMins , secs ) = rawSecs  `divMod` 60
    (rawHours, mins ) = rawMins  `divMod` 60
    (days    , hours) = rawHours `divMod` 24

-- | Run a countdown on the console.
countdownConsole
    :: MonadIO m
    => Finite 25        -- ^ day to count down to
    -> m a              -- ^ callback on release
    -> m a
countdownConsole d = countdownWith d 250000 $ \ttr -> liftIO $ do
    ANSI.clearLine
    printf "> Day %d release in: %s" (dayToInt d) (showNominalDiffTime ttr)
    ANSI.setCursorColumn 0
    hFlush stdout

-- | Run a countdown with a given callback on each tick.
countdownWith
    :: MonadIO m
    => Finite 25                    -- ^ day to count down to
    -> Int                          -- ^ interval (milliseconds)
    -> (NominalDiffTime -> m ())    -- ^ callback on each tick
    -> m a                          -- ^ callback on release
    -> m a
countdownWith d delay callback release = go
  where
    go = do
      ttr <- liftIO $ timeToRelease d
      if ttr <= 0
        then release
        else do
          callback ttr
          liftIO $ threadDelay delay
          go

