{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : AOC2018.Interactive
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Run actions regarding challenges, solutions, tests, submissions, viewing
-- prompts, etc.
--
-- Essentially implements the functionality of the main app.
--

module AOC2018.Run (
    TestSpec(..)
  , MainRunOpts(..), HasMainRunOpts(..), mainRun, defaultMRO
  , MainViewOpts(..), HasMainViewOpts(..), mainView
  , MainSubmitOpts(..), HasMainSubmitOpts(..), mainSubmit, defaultMSO
  , withColor
  ) where

import           AOC2018.API
import           AOC2018.Challenge
import           AOC2018.Run.Config
import           AOC2018.Run.Load
import           AOC2018.Solver
import           AOC2018.Util
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Criterion
import           Data.Bifunctor
import           Data.Char
import           Data.Finite
import           Data.Map                 (Map)
import           Data.Maybe
import           Data.Time
import           Text.Printf
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified System.Console.ANSI      as ANSI
import qualified System.Console.Haskeline as H

data TestSpec = TSAll
              | TSDayAll  { _tsDay  :: Finite 25     }
              | TSDayPart { _tsSpec :: ChallengeSpec }
  deriving Show

data MainRunOpts = MRO { _mroSpec  :: !TestSpec
                       , _mroTest  :: !Bool
                       , _mroBench :: !Bool
                       , _mroLock  :: !Bool
                       }
  deriving Show

makeClassy ''MainRunOpts

newtype MainViewOpts = MVO { _mvoSpec :: ChallengeSpec
                           }
  deriving Show

makeClassy ''MainViewOpts

data MainSubmitOpts = MSO { _msoSpec  :: !ChallengeSpec
                          , _msoTest  :: !Bool
                          , _msoForce :: !Bool
                          , _msoLock  :: !Bool
                          }
  deriving Show

makeClassy ''MainSubmitOpts

defaultMRO :: TestSpec -> MainRunOpts
defaultMRO ts = MRO { _mroSpec  = ts
                    , _mroTest  = False
                    , _mroBench = False
                    , _mroLock  = False
                    }

defaultMSO :: ChallengeSpec -> MainSubmitOpts
defaultMSO cs = MSO { _msoSpec  = cs
                    , _msoTest  = True
                    , _msoForce = False
                    , _msoLock  = True
                    }

mainRun
    :: (MonadIO m, MonadError [String] m)
    => Config
    -> MainRunOpts
    -> m ()
mainRun Cfg{..} MRO{..} =  do
    toRun <- case _mroSpec of
      TSAll      -> pure challengeMap
      TSDayAll d -> maybeToEither [printf "Day not yet avaiable: %s" (showDay d)] $
                       M.singleton d <$> M.lookup d challengeMap
      TSDayPart (CS d p) -> do
        ps <- maybeToEither [printf "Day not yet available: %s" (showDay d)] $
                M.lookup d challengeMap
        c  <- maybeToEither [printf "Part not found: %c" p] $
                M.lookup p ps
        pure $ M.singleton d (M.singleton p c)
    
    void . liftIO . flip (runAll _cfgSession _mroLock) toRun $ \c CD{..} -> do
      when _mroTest $ do
        testRes <- mapMaybe fst <$> mapM (uncurry (testCase True c)) _cdTests
        unless (null testRes) $ do
          let (mark, color)
                  | and testRes = ('✓', ANSI.Green)
                  | otherwise   = ('✗', ANSI.Red  )
          withColor ANSI.Vivid color $
            printf "[%c] Passed %d out of %d test(s)\n"
                mark
                (length (filter id testRes))
                (length testRes)

      case _cdInput of
        Right inp
          | _mroBench -> benchmark (nf (runSomeSolution c) inp)
          | otherwise -> void $ testCase False c inp _cdAnswer
        Left e
          | _mroTest  -> pure ()
          | otherwise -> putStrLn "[INPUT ERROR]" *> mapM_ putStrLn e

mainView
    :: (MonadIO m, MonadError [String] m)
    => Config
    -> MainViewOpts
    -> m ()
mainView Cfg{..} MVO{..} = do
    CD{..} <- liftIO $ challengeData _cfgSession _mvoSpec
    pmpt   <- liftEither . first ("[PROMPT ERROR]":) $ _cdPrompt
    liftIO $ do
      putStrLn pmpt
      putStrLn ""

mainSubmit
    :: (MonadIO m, MonadError [String] m)
    => Config
    -> MainSubmitOpts
    -> m ()
mainSubmit Cfg{..} MSO{..} = do
    CD{..} <- liftIO $ challengeData _cfgSession _msoSpec
    dMap   <- maybeToEither [printf "Day not yet available: %d" d'] $
                M.lookup _csDay challengeMap
    c      <- maybeToEither [printf "Part not found: %c" _csPart] $
                M.lookup _csPart dMap
    inp    <- liftEither . first ("[PROMPT ERROR]":) $ _cdInput
    sess   <- HasKey <$> maybeToEither ["ERROR: Session Key Required to Submit"] _cfgSession

    when _msoTest $ do
      testRes <- liftIO $
          mapMaybe fst <$> mapM (uncurry (testCase True c)) _cdTests
      unless (null testRes) $ do
        let (mark, color)
                | and testRes = ('✓', ANSI.Green)
                | otherwise   = ('✗', ANSI.Red  )
        liftIO $ do
          withColor ANSI.Vivid color $
            printf "[%c] Passed %d out of %d test(s)\n"
                mark
                (length (filter id testRes))
                (length testRes)
        unless (and testRes) $ do
          if _msoForce
            then liftIO $ putStrLn "Proceeding with submission despite test failures (--force)"
            else do
              conf <- liftIO . H.runInputT H.defaultSettings $
                H.getInputChar "Some tests failed. Are you sure you wish to proceed? y/(n)"
              case toLower <$> conf of
                Just 'y' -> pure ()
                _        -> throwError ["Submission aborted."]

    resEither <- liftIO . evaluate . force . runSomeSolution c $ inp
    res       <- liftEither . first (("[SOLUTION ERROR]":) . (:[]) . show) $ resEither
    liftIO $ printf "Submitting solution: %s\n" res

    (resp, status) <- liftEither =<< liftIO (runAPI sess (ASubmit _csDay _csPart res))
    let (color, lock, out) = case status of
          SubCorrect -> (ANSI.Green  , True , "Answer was correct!"          )
          SubWrong   -> (ANSI.Red    , False, "Answer was incorrect!"        )
          SubWait    -> (ANSI.Yellow , False, "Answer re-submitted too soon.")
          SubInvalid -> (ANSI.Blue   , False, "Submission was rejected.  Maybe not unlocked yet, or already answered?")
          SubUnknown -> (ANSI.Magenta, False, "Response from server was not recognized.")
    liftIO $ do
      withColor ANSI.Vivid color $
        putStrLn out
      T.putStrLn resp
      when lock $
        if _msoLock
          then putStrLn "Locking correct answer." >> writeFile _cpAnswer res
          else putStrLn "Not locking correct answer (--no-lock)"
      zt <- getZonedTime
      appendFile _cpLog $ printf logFmt (show zt) res (showSubmitRes status) (formatResp resp)
  where
    CS{..} = _msoSpec
    CP{..} = challengePaths _msoSpec
    d' = getFinite _csDay + 1
    formatResp = T.unpack . T.intercalate "\n" . map ("> " <>) . T.lines
    logFmt = unlines [ "[%s]"
                        , "Submission: %s"
                        , "Status: %s"
                        , "%s"
                        ]

runAll
    :: Maybe String       -- ^ session key
    -> Bool               -- ^ run and lock answer
    -> (SomeSolution -> ChallengeData -> IO a)
    -> ChallengeMap
    -> IO (Map (Finite 25) (Map Char a))
runAll sess lock f = M.traverseWithKey $ \d ->
                     M.traverseWithKey $ \p c -> do
    let CP{..} = challengePaths (CS d p)
    withColor ANSI.Dull ANSI.Blue $
      printf ">> Day %02d%c\n" (getFinite d + 1) p
    when lock $ do
      CD{..} <- challengeData sess (CS d p)
      forM_ _cdInput $ \inp ->
        mapM_ (writeFile _cpAnswer) =<< evaluate (force (runSomeSolution c inp))
    f c =<< challengeData sess (CS d p)

testCase
    :: Bool             -- ^ is just an example
    -> SomeSolution
    -> String
    -> Maybe String
    -> IO (Maybe Bool, Either SolutionError String)
testCase emph c inp ans = do
    withColor ANSI.Dull color $
      printf "[%c]" mark
    if emph
      then printf " (%s)\n" resStr
      else printf " %s\n"   resStr
    forM_ showAns $ \a -> do
      withColor ANSI.Vivid ANSI.Red $
        printf "(Expected: %s)\n" a
    return (status, res)
  where
    res = runSomeSolution c inp
    resStr = case res of
      Right r -> r
      Left SEParse -> "ERROR: No parse"
      Left SESolve -> "ERROR: No solution"
    (mark, showAns, status) = case ans of
      Just (strip->ex)    -> case res of
        Right (strip->r)
          | r == ex   -> ('✓', Nothing, Just True )
          | otherwise -> ('✗', Just ex, Just False)
        Left _        -> ('✗', Just ex, Just False)
      Nothing             -> ('?', Nothing, Nothing   )
    color = case status of
      Just True  -> ANSI.Green
      Just False -> ANSI.Red
      Nothing    -> ANSI.Blue

withColor
    :: ANSI.ColorIntensity
    -> ANSI.Color
    -> IO ()
    -> IO ()
withColor ci c act = do
    ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ci c ]
    act
    ANSI.setSGR [ ANSI.Reset ]
