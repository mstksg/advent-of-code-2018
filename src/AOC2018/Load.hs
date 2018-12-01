{-# LANGUAGE OverloadedStrings #-}

module AOC2018.Load (
    ChallengePaths(..), challengePaths
  , ChallengeData(..), challengeData
  ) where

import           AOC2018.Types              as AOC
import           AOC2018.Util               as AOC
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens hiding        ((<.>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Finite
import           Data.Foldable
import           Data.List
import           Data.Text.Lens
import           Network.Curl
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           Text.Printf
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Text.Pandoc                as P
import qualified Text.Taggy                 as H
import qualified Text.Taggy.Lens            as H


-- | A record of paths corresponding to a specific challenge.
data ChallengePaths = CP { _cpPromptUrl :: !FilePath
                         , _cpDataUrl   :: !FilePath
                         , _cpPrompt    :: !FilePath
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
    { _cpPromptUrl = printf "https://adventofcode.com/2018/day/%d" d'
    , _cpDataUrl   = printf "https://adventofcode.com/2018/day/%d/input" d'
    , _cpPrompt    = "prompt"    </> printf "%02d%c" d' p <.> "txt"
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
    fetchUrl :: FilePath -> ExceptT [String] IO String
    fetchUrl u = do
      s <- maybe (throwE ["Session key needed to fetch input"]) return
        sess
      (cc, r) <- liftIO . withCurlDo . curlGetString u $
          CurlCookie (printf "session=%s" s) : method_GET
      case cc of
        CurlOK -> return ()
        _      -> throwE [ "Error contacting advent of code server to fetch input"
                         , "Possible invalid session key"
                         , printf "Url: %s" u
                         , printf "Server response: %s" r
                         ]
      return r
    fetchInput :: ExceptT [String] IO String
    fetchInput = do
      r <- fetchUrl _cpDataUrl
      liftIO $ writeFile _cpInput r
      return r
    fetchPrompt :: ExceptT [String] IO String
    fetchPrompt = do
      rHtml <- fetchUrl _cpPromptUrl
      let pmts = M.fromList . zip ['a'..] . processPrompt $ rHtml
      pMaybe <- maybe (throwE ["Part not yet released"]) pure
              . M.lookup (_csPart spec)
              $ pmts
      pmt    <- either throwE pure pMaybe
      liftIO $ T.writeFile _cpPrompt pmt
      return $ T.unpack pmt
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

processPrompt :: String -> [Either [String] T.Text]
processPrompt html = runExceptT $ do
    article <- lift $ html ^.. packed . H.html
                             . H.allNamed (only "article")
                             . to (set H.name "body")
                             . to H.NodeElement
    let articleText = unpacked . packed . H.html # article
    either (throwE . (:[]) . show) pure . P.runPure $ do
      p <- P.readHtml (P.def { P.readerExtensions = exts })
              articleText
      P.writeMarkdown (P.def { P.writerExtensions = exts }) p
  where
    exts = P.disableExtension P.Ext_header_attributes
         . P.disableExtension P.Ext_smart
         $ P.pandocExtensions


