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
    execSolution
  , execSolutionWith
  , testSolution
  , viewPrompt
  , mkSpec
  ) where

-- import           Data.Bifunctor
import           AOC2018.Challenge
import           AOC2018.Config
import           AOC2018.Load
import           AOC2018.Solver
import           AOC2018.Util
import           Control.Monad
import           Data.Finite
import           Text.Printf
import qualified Data.Map          as M

-- | Run the solution indicated by the challenge spec on the official
-- puzzle input.
execSolution :: ChallengeSpec -> IO ()
execSolution cs = do
    Cfg{..} <- configFile "aoc-conf.yaml"
    c       <- case lookupSolution cs challengeMap of
      Nothing -> fail "Solution not yet implemented."
      Just c  -> pure c
    CD{..} <- challengeData _cfgSession cs
    case _cdInput of
      Right inp -> case runSomeSolution c inp of
        Right res -> putStrLn res
        Left  e   -> print e
      Left  e   -> mapM_ putStrLn e

-- | Run the solution indicated by the challenge spec on a custom input.
execSolutionWith
    :: ChallengeSpec
    -> String               -- ^ custom puzzle input
    -> IO ()
execSolutionWith cs inp = do
    Cfg{..} <- configFile "aoc-conf.yaml"
    c       <- case lookupSolution cs challengeMap of
      Nothing -> fail "Solution not yet implemented."
      Just c  -> pure c
    case runSomeSolution c inp of
      Right res -> putStrLn res
      Left  e   -> print e

-- | Run test suite for a given challenge spec.
testSolution :: ChallengeSpec -> IO ()
testSolution cs@CS{..} = do
    Cfg{..} <- configFile "aoc-conf.yaml"
    c       <- case M.lookup _csPart <=< M.lookup _csDay $ challengeMap of
      Nothing -> fail "Challenge not implemented."
      Just c  -> pure c
    CD{..} <- challengeData _cfgSession cs
    forM_ (zip [1..] _cdTests) $ \(i :: Int, (inp, ans)) ->
      case ans of
        Just (strip->ex) -> case runSomeSolution c inp of
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
      Left  e -> mapM_ putStrLn e
      Right p -> putStrLn p >> putStrLn ""

-- submitSolution :: ChallengeSpec -> IO ()
-- submitSolution cs = do
--     Cfg{..} <- configFile "aoc-conf.yaml"
--     c       <- case lookupSolution cs challengeMap of
--       Nothing -> fail "Solution not yet implemented."
--       Just c  -> pure c
--     CD{..} <- challengeData _cfgSession cs
--     res <- either (fail . unlines) pure
--          $ first ((:[]) . show) . runSomeSolution c =<< _cdInput
--     _

-- mainSubmit :: Config -> ChallengeSpec -> Bool -> Bool -> Bool -> IO (Either [String] ())
-- mainSubmit Cfg{..} spec@CS{..} test force' noLock = runExceptT $ do
--     CD{..} <- liftIO $ challengeData _cfgSession spec
--     dMap   <- maybeToEither [printf "Day not yet available: %d" d'] $
--                 M.lookup _csDay challengeMap
--     c      <- maybeToEither [printf "Part not found: %c" _csPart] $
--                 M.lookup _csPart dMap
--     inp    <- liftEither . first ("[PROMPT ERROR]":) $ _cdInput
--     sess   <- HasKey <$> maybeToEither ["ERROR: Session Key Required to Submit"] _cfgSession

--     when test $ do
--       testRes <- liftIO $
--           mapMaybe fst <$> mapM (uncurry (testCase True c)) _cdTests
--       unless (null testRes) $ do
--         let (mark, color)
--                 | and testRes = ('✓', ANSI.Green)
--                 | otherwise   = ('✗', ANSI.Red  )
--         liftIO $ do
--           withColor ANSI.Vivid color $
--             printf "[%c] Passed %d out of %d test(s)\n"
--                 mark
--                 (length (filter id testRes))
--                 (length testRes)
--         unless (and testRes) $ do
--           if force'
--             then liftIO $ putStrLn "Proceeding with submission despite test failures (--force)"
--             else do
--               conf <- liftIO . H.runInputT H.defaultSettings $
--                 H.getInputChar "Some tests failed. Are you sure you wish to proceed? y/(n)"
--               case toLower <$> conf of
--                 Just 'y' -> pure ()
--                 _        -> throwError ["Submission aborted."]

--     resEither <- liftIO . evaluate . force . runSomeSolution c $ inp
--     res       <- liftEither . first (("[SOLUTION ERROR]":) . (:[]) . show) $ resEither
--     liftIO $ printf "Submitting solution: %s\n" res

--     (resp, status) <- ExceptT $ runAPI sess (ASubmit _csDay _csPart res)
--     let (color, lock, out) = case status of
--           SubCorrect -> (ANSI.Green  , True , "Answer was correct!"          )
--           SubWrong   -> (ANSI.Red    , False, "Answer was incorrect!"        )
--           SubWait    -> (ANSI.Yellow , False, "Answer re-submitted too soon.")
--           SubInvalid -> (ANSI.Blue   , False, "Submission was rejected.  Maybe not unlocked yet, or already answered?")
--           SubUnknown -> (ANSI.Magenta, False, "Response from server was not recognized.")
--     liftIO $ do
--       withColor ANSI.Vivid color $
--         putStrLn out
--       T.putStrLn resp
--       when lock $
--         if noLock
--           then putStrLn "Not locking correct answer (--no-lock)"
--           else putStrLn "Locking correct answer." >> writeFile _cpAnswer res
--       zt <- getZonedTime
--       appendFile _cpLog $ printf logFmt (show zt) res (showSubmitRes status) (formatResp resp)

-- | Unsafely create a 'ChallengeSpec' from a day number and part.
--
-- Is undefined if given a day number out of range (1-25).
mkSpec :: Integer -> Char -> ChallengeSpec
mkSpec i c = maybe e (`CS` c) . packFinite $ i - 1
  where
    e = errorWithoutStackTrace $ printf "Day out of range: %d" i

