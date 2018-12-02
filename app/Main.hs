import           AOC2018
import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Criterion
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Options.Applicative
import           Text.Printf
import           Text.Read
import qualified Data.Map            as M
import qualified System.Console.ANSI as ANSI

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
                    , _mTest     :: Bool
                    }

data Opts = O { _oMode   :: Mode
              , _oConfig :: Maybe FilePath
              }

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
      MSubmit{..} -> mainSubmit cfg _mSpec _mTest

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
          ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Vivid color ]
          printf "[%c] Passed %d out of %d test(s)\n"
              mark
              (length (filter id testRes))
              (length testRes)
          ANSI.setSGR [ ANSI.Reset ]

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
        ps <- maybe (Left $ printf "Day not yet available: %d" (getFinite d + 1)) Right $
                M.lookup d challengeMap
        c  <- maybe (Left $ printf "Part not found: %c" p) Right $
                M.lookup p ps
        return $ M.singleton d (M.singleton p c)

mainSubmit :: Config -> ChallengeSpec -> Bool -> IO ()
mainSubmit Cfg{..} spec test = putStrLn "Unimplemented"
--     CD{..} <- challengeData _cfgSession spec
--     when test $ do
--       testRes <- mapMaybe fst <$> mapM (uncurry (testCase True c)) _cdTests
--       unless (null testRes) $ do
--         let (mark, color)
--                 | and testRes = ('✓', ANSI.Green)
--                 | otherwise   = ('✗', ANSI.Red  )
--         ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Vivid color ]
--         printf "[%c] Passed %d out of %d test(s)\n"
--             mark
--             (length (filter id testRes))
--             (length testRes)
--         ANSI.setSGR [ ANSI.Reset ]

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
    ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Blue ]
    printf ">> Day %02d%c\n" (getFinite d + 1) p
    ANSI.setSGR [ ANSI.Reset ]
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
    ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Dull color ]
    printf "[%c]" mark
    ANSI.setSGR [ ANSI.Reset ]
    if emph
      then printf " (%s)\n" resStr
      else printf " %s\n"   resStr
    forM_ showAns $ \a -> do
      ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red ]
      printf "(Expected: %s)\n" a
      ANSI.setSGR [ ANSI.Reset ]
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
        t <- switch $ long "test"
                   <> short 't'
                   <> help "Run sample tests"
        pure $ MSubmit s t

