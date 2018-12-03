{-# LANGUAGE OverloadedStrings              #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

import           AOC2018
import           Control.Applicative
import           Control.Lens hiding         (argument)
import           Control.Monad
import           Control.Monad.Except
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Options.Applicative
import           Text.Read
import qualified Data.Map                    as M
import qualified System.Console.ANSI         as ANSI

data Mode = MRun    MainRunOpts
          | MView   MainViewOpts
          | MSubmit MainSubmitOpts

-- TODO: countdowner

data Opts = O { _oMode   :: !Mode
              , _oConfig :: !(Maybe FilePath)
              }

main :: IO ()
main = do
    O{..} <- execParser $ info (parseOpts <**> helper)
                ( fullDesc
               <> header "aoc2018 - Advent of Code 2018 challenge runner"
               <> progDesc ("Run challenges from Advent of Code 2018. Available days: " ++ availableDays)
                )
    cfg@Cfg{..} <- configFile $ fromMaybe "aoc-conf.yaml" _oConfig
    out <- runExceptT $ case _oMode of
      MRun    mro -> void $ mainRun  cfg mro
      MView   mvo -> void $ mainView cfg mvo
      MSubmit mso -> void $ mainSubmit cfg mso
    forOf_ _Left out $ \e -> do
      withColor ANSI.Vivid ANSI.Red $
        putStrLn "[ERROR]"
      mapM_ putStrLn e
  where
    availableDays = intercalate ", "
                  . map (show . dayToInt)
                  . M.keys
                  $ challengeMap


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
        Just p' -> TSDayPart (CS d' p')
        Nothing -> TSDayAll  d'
      Nothing -> TSAll
  where
    pDay = asum [ Nothing <$ maybeReader (guard . (== "all") . map toLower)
                , Just <$> readFinite
                ]
    pPart = readPart

parseOpts :: Parser Opts
parseOpts = do
    _oConfig <- optional . strOption . mconcat $
      [ long "config"
      , short 'c'
      , metavar "PATH"
      , help "Path to configuration file (default: aoc-conf.yaml)"
      ]
    _oMode <- subparser . mconcat $
      [ command "run"    (info (MRun    <$> parseRun   ) (progDesc "Run, test, and benchmark challenges"   ))
      , command "view"   (info (MView   <$> parseView  ) (progDesc "View a prompt for a given challenge"   ))
      , command "submit" (info (MSubmit <$> parseSubmit) (progDesc "Test and submit answers for challenges"))
      , command "test"   (info (MRun    <$> parseTest  ) (progDesc "Alias for run --test"                  ))
      , command "bench"  (info (MRun    <$> parseBench ) (progDesc "Alias for run --bench"                 ))
      ]
    pure O{..}
  where
    parseRun    :: Parser MainRunOpts
    parseRun = do
        _mroSpec <- parseTestSpec
        _mroTest <- switch . mconcat $
          [ long "test"
          , short 't'
          , help "Run sample tests"
          ]
        _mroBench <- switch . mconcat $
          [ long "bench"
          , short 'b'
          , help "Run benchmarks"
          ]
        _mroLock <- switch . mconcat $
          [ long "lock"
          , short 'l'
          , help "Lock in results as \"correct\" answers"
          ]
        pure $ let _mroInput = M.empty
               in  MRO{..}
    parseView   :: Parser MainViewOpts
    parseView = MVO <$> parseTestSpec
    parseSubmit :: Parser MainSubmitOpts
    parseSubmit = do
        _msoSpec <- parseChallengeSpec
        _msoTest <- fmap not . switch . mconcat $
          [ long "skip-tests"
          , short 's'
          , help "Skip running tests before submission"
          ]
        _msoForce <- switch . mconcat $
          [ long "force"
          , short 'f'
          , help "Always submit, even if tests fail"
          ]
        _msoLock <- fmap not . switch . mconcat $
          [ long "no-lock"
          , short 'n'
          , help "Do not lock in answer, even if correct submission was received"
          ]
        pure MSO{..}
    parseTest  :: Parser MainRunOpts
    parseTest  = parseRun & mapped . mroTest  .~ True
    parseBench :: Parser MainRunOpts
    parseBench = parseRun & mapped . mroBench .~ True

