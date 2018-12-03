{-# LANGUAGE TemplateHaskell                #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

import           AOC2018
import           Control.Applicative
import           Control.Lens hiding (argument)
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Criterion
import           Data.Bifunctor
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Options.Applicative
import           Text.Printf
import           Text.Read
import qualified Data.Map                 as M
import qualified Data.Text.IO             as T
import qualified System.Console.ANSI      as ANSI
import qualified System.Console.Haskeline as H

data TestSpec = TSAll
              | TSDayAll  { _tsDay  :: Finite 25 }
              | TSDayPart { _tsDay  :: Finite 25
                          , _tsPart :: Char
                          }
  deriving Show

data Mode = MRun    { _mTestSpec :: TestSpec
                    , _mTest     :: Bool
                    , _mBench    :: Bool
                    , _mLock     :: Bool
                    }
          | MView   { _mSpec     :: ChallengeSpec
                    }
          | MSubmit { _mSpec     :: ChallengeSpec
                    , _mNoTest   :: Bool
                    , _mForce    :: Bool
                    , _mNoLock   :: Bool
                    }

data Opts = O { _oMode   :: Mode
              , _oConfig :: Maybe FilePath
              }

makeLenses ''Mode

main :: IO ()
main = do
    O{..} <- execParser $ info (parseOpts <**> helper)
                ( fullDesc
               <> header "aoc2018 - Advent of Code 2018 challenge runner"
               <> progDesc ("Run challenges from Advent of Code 2018. Available days: " ++ availableDays)
                )
    cfg@Cfg{..} <- configFile $ fromMaybe "aoc-conf.yaml" _oConfig
    case _oMode of
      MRun{..} -> mainRun cfg _mTestSpec _mTest _mBench _mLock
      MView{..} -> do
        CD{..} <- challengeData _cfgSession _mSpec
        case _cdPrompt of
          Left err -> do
            putStrLn "[PROMPT ERROR]"
            mapM_ (putStrLn . ("  " ++)) err
          Right pmpt -> putStrLn pmpt >> putStrLn ""
      MSubmit{..} -> mainSubmit cfg _mSpec _mNoTest _mForce _mNoLock >>= \case
        Left   e -> mapM_ putStrLn e
        Right () -> pure ()

  where
    availableDays = intercalate ", "
                  . map (show . (+ 1) . getFinite)
                  . M.keys
                  $ challengeMap


mainRun :: Config -> TestSpec -> Bool -> Bool -> Bool -> IO ()
mainRun Cfg{..} testSpec test bench' lock = case toRun of
    Left e   -> putStrLn e
    Right cs -> flip (runAll _cfgSession lock) cs $ \c CD{..} -> do

      case _cdInput of
        Left err | not test || bench' -> do
          putStrLn "[INPUT ERROR]"
          mapM_ (putStrLn . ("  " ++)) err
        _ ->
          return ()

      when test $ do
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

      forM_ _cdInput $ \inp ->
        if bench'
          then void $ benchmark (nf (runSomeSolution c) inp)
          else void $ testCase False c inp _cdAnswer
  where
    toRun = case testSpec of
      TSAll -> Right challengeMap
      TSDayAll d ->
        case M.lookup d challengeMap of
          Nothing -> Left  $ printf "Day not yet available: %d" (getFinite d + 1)
          Just cs -> Right $ M.singleton d cs
      TSDayPart d p -> do
        ps <- maybeToEither (printf "Day not yet available: %d" (getFinite d + 1)) $
                M.lookup d challengeMap
        c  <- maybeToEither (printf "Part not found: %c" p) $
                M.lookup p ps
        return $ M.singleton d (M.singleton p c)

mainSubmit :: Config -> ChallengeSpec -> Bool -> Bool -> Bool -> IO (Either [String] ())
mainSubmit Cfg{..} spec@CS{..} test force' noLock = runExceptT $ do
    CD{..} <- liftIO $ challengeData _cfgSession spec
    dMap   <- maybeToEither [printf "Day not yet available: %d" d'] $
                M.lookup _csDay challengeMap
    c      <- maybeToEither [printf "Part not found: %c" _csPart] $
                M.lookup _csPart dMap
    inp    <- liftEither . first ("[PROMPT ERROR]":) $ _cdInput
    sess   <- HasKey <$> maybeToEither ["ERROR: Session Key Required to Submit"] _cfgSession

    when test $ do
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
          if force'
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

    (resp, status) <- ExceptT $ runAPI sess (ASubmit _csDay _csPart res)
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
        if noLock
          then putStrLn "Not locking correct answer (--no-lock)"
          else putStrLn "Locking correct answer." >> writeFile _cpAnswer res
  where
    CP{..} = challengePaths spec
    d' = getFinite _csDay + 1

runAll
    :: Maybe String       -- ^ session key
    -> Bool               -- ^ run and lock answer
    -> (SomeSolution -> ChallengeData -> IO ())
    -> ChallengeMap
    -> IO ()
runAll sess lock f = fmap void         $
                     M.traverseWithKey $ \d ->
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

-- ---------
-- | Parsers
-- ---------

readFinite :: ReadM (Finite 25)
readFinite = eitherReader $ \s -> do
    n <- maybe (Left "Invalid day") Right $ readMaybe s
    maybe (Left "Day out of range") Right $ packFinite (n - 1)

readPart :: ReadM Char
readPart = eitherReader $ \case
    []  -> Left "No part"
    [p] | isAlpha p -> Right (toLower p)
        | otherwise -> Left "Invalid part (not an alphabet letter)"
    _   -> Left "Invalid part (not a single alphabet letter)"

parseChallengeSpec :: Parser ChallengeSpec
parseChallengeSpec = do
    d <- argument pDay ( metavar "DAY"
                      <> help "Day of challenge (1 - 25)"
                       )
    p <- argument pPart ( metavar "PART"
                      <> help "Challenge part (a, b, c, etc.)"
                        )
    pure $ CS d p
  where
    pDay  = readFinite
    pPart = readPart

parseTestSpec :: Parser TestSpec
parseTestSpec = do
    d <- argument pDay ( metavar "DAY"
                      <> help "Day of challenge (1 - 25), or \"all\""
                       )
    p <- optional $ argument pPart ( metavar "PART"
                                  <> help "Challenge part (a, b, c, etc.)"
                                   )
    pure $ case d of
      Just d' -> case p of
        Just p' -> TSDayPart d' p'
        Nothing -> TSDayAll  d'
      Nothing -> TSAll
  where
    pDay = asum [ Nothing <$ maybeReader (guard . (== "all") . map toLower)
                , Just <$> readFinite
                ]
    pPart = readPart

parseOpts :: Parser Opts
parseOpts = do
    c <- optional . strOption $
         long "config"
      <> short 'c'
      <> metavar "PATH"
      <> help "Path to configuration file (default: aoc2018-conf.yaml)"
    m <- subparser $
         command "run"    (info parseRun    (progDesc "Run, test, and benchmark challenges"   ))
      <> command "view"   (info parseView   (progDesc "View a prompt for a given challenge"   ))
      <> command "submit" (info parseSubmit (progDesc "Test and submit answers for challenges"))
      <> command "test"   (info parseTest   (progDesc "Alias for run --test"))
      <> command "bench"  (info parseBench  (progDesc "Alias for run --bench"))
    pure O { _oMode   = m
           , _oConfig = c
           }
  where
    parseRun    :: Parser Mode
    parseRun = do
        s <- parseTestSpec
        t <- switch $ long "test"
                   <> short 't'
                   <> help "Run sample tests"
        b <- switch $ long "bench"
                   <> short 'b'
                   <> help "Run benchmarks"
        l <- switch $ long "lock"
                   <> short 'l'
                   <> help "Lock in results as \"correct\" answers"
        pure $ MRun s t b l
    parseView   :: Parser Mode
    parseView = MView <$> parseChallengeSpec
    parseSubmit :: Parser Mode
    parseSubmit = do
        s <- parseChallengeSpec
        t <- switch $ long "skip-tests"
                   <> short 's'
                   <> help "Skip running tests before submission"
        f <- switch $ long "force"
                   <> short 'f'
                   <> help "Always submit, even if tests fail"
        n <- switch $ long "no-lock"
                   <> short 'n'
                   <> help "Do not lock in answer, even if correct submission was received"
        pure $ MSubmit s t f n
    parseTest  :: Parser Mode
    parseTest  = parseRun & mapped . mTest  .~ True
    parseBench :: Parser Mode
    parseBench = parseRun & mapped . mBench .~ True

