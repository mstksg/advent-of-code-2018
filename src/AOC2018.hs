{-# LANGUAGE TemplateHaskell #-}

module AOC2018 (
    module AOC
  , challengeMap
  , ChallengePaths(..), challengePaths
  , ChallengeData(..), challengeData
  , Config(..), configFile, defConfPath
  , session
  ) where

import           AOC2018.Challenge.Day01 as AOC
-- import           AOC2018.Challenge.Day02 as AOC
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

import           AOC2018.Discover
import           AOC2018.Types              as AOC
import           AOC2018.Util               as AOC
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Finite
import           Data.Foldable
import           Data.List
import           Data.Map                   (Map)
import           GHC.Generics               (Generic)
import           Network.Curl
import           System.FilePath
import           System.IO.Error
import           Text.Printf
import qualified Data.Aeson                 as A
import qualified Data.ByteString            as BS
import qualified Data.Yaml                  as Y

challengeMap :: Map (Finite 25) (Map Char Challenge)
challengeMap = mkChallengeMap $$(challengeList "src/AOC2018/Challenge")

data ChallengePaths = CP { _cpDataUrl :: !FilePath
                         , _cpInput   :: !FilePath
                         , _cpAnswer  :: !FilePath
                         , _cpTests   :: !FilePath
                         }
  deriving Show

data ChallengeData = CD { _cdInp   :: !(Either [String] String)
                        , _cdAns   :: !(Maybe String)
                        , _cdTests :: ![(String, Maybe String)]
                        }

challengePaths :: ChallengeSpec -> ChallengePaths
challengePaths (CS d p) = CP
    { _cpDataUrl = printf "http://adventofcode.com/2018/day/%d/input" d'
    , _cpInput   = "data"     </> printf "%02d" d' <.> "txt"
    , _cpAnswer  = "data/ans" </> printf "%02d%c" d' p <.> "txt"
    , _cpTests   = "test-data" </> printf "%02d%c" d' p <.> "txt"
    }
  where
    d' = getFinite d + 1

challengeData
    :: Maybe String
    -> ChallengeSpec
    -> IO ChallengeData
challengeData sess spec = do
    inp   <- runExceptT . asum $
      [ ExceptT $ maybe (Left [fileErr]) Right <$> readFileMaybe _cpInput
      , fetchInput
      ]
    ans   <- readFileMaybe _cpAnswer
    ts    <- foldMap (parseTests . lines) <$> readFileMaybe _cpTests
    return $ CD inp ans ts
  where
    CP{..} = challengePaths spec
    fileErr = printf "Input file not found at %s" _cpInput
    readFileMaybe :: FilePath -> IO (Maybe String)
    readFileMaybe =
        (traverse (evaluate . force) . either (const Nothing) Just =<<)
       . tryJust (guard . isDoesNotExistError)
       . readFile
    fetchInput :: ExceptT [String] IO String
    fetchInput = do
      s <- maybe (throwE ["Session key needed to fetch input"]) return
        sess
      (cc, r) <- liftIO . withCurlDo . curlGetString _cpDataUrl $
          CurlCookie (printf "session=%s" s) : method_GET
      case cc of
        CurlOK -> return ()
        _      -> throwE [ "Error contacting advent of code server to fetch input"
                         , "Possible invalid session key"
                         , printf "Url: %s" _cpDataUrl
                         , printf "Server response: %s" r
                         ]
      liftIO $ writeFile _cpInput r
      return r
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

newtype Config = Cfg { _cfgSession :: Maybe String }
  deriving (Generic)

defConfPath :: FilePath
defConfPath = "aoc-conf.yaml"

configFile :: FilePath -> IO Config
configFile fp = do
    cfgInp <- tryJust (guard . isDoesNotExistError)
            $ BS.readFile fp
    case cfgInp of
      Left () -> do
        Y.encodeFile fp emptyCfg
        return emptyCfg
      Right b -> do
        case Y.decodeEither' b of
          Left e -> do
            printf "Configuration file at %s could not be parsed:\n" fp
            print e
            return emptyCfg
          Right cfg -> return cfg
  where
    emptyCfg = Cfg Nothing

session :: FilePath -> IO (Maybe String)
session = fmap _cfgSession . configFile

configJSON :: A.Options
configJSON = A.defaultOptions
    { A.fieldLabelModifier = A.camelTo2 '-' . drop 4 }

instance A.ToJSON Config where
    toJSON     = A.genericToJSON configJSON
    toEncoding = A.genericToEncoding configJSON
instance A.FromJSON Config where
    parseJSON  = A.genericParseJSON configJSON

