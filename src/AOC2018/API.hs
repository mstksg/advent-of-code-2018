{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC2018.API
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell bindings for Advent of Code 2018 REST API.
--

module AOC2018.API (
  -- -- * API
  --   API(..), SubmitRes(..), showSubmitRes
  -- , runAPI
  -- -- * Session Keys
  -- , SessionKey(..)
  -- , sessionKey
  -- , sessionKey_
  -- -- * Util
  -- , parseSubmitRes
  ) where

import           AOC2018.Util
import           Control.Applicative.Lift
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Char
import           Data.Finite
import           Data.Kind
import           Data.Map                 (Map)
import           Data.Maybe
import           Data.Text                (Text)
import           Data.Text.Lens
import           Network.Curl
import           Text.Printf
import           Text.Read (readMaybe)
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Text.Pandoc              as P
import qualified Text.Taggy               as H
import qualified Text.Taggy.Lens          as H

-- | The result of a submission.
data SubmitRes = SubCorrect (Maybe Int)     -- ^ global rank
               | SubIncorrect
               | SubWait
               | SubInvalid
               | SubUnknown
  deriving (Show, Eq, Ord)

-- | An API command.  An @'API' k a@ an AoC API request that returns
-- results of type @a@; if @k@ is ''True', it requires a session key.  If
-- @k@ is ''False', it does not.
data API :: Bool -> Type -> Type where
    -- | Fetch prompts, as Markdown.
    APrompt
        :: Finite 25                    -- ^ Day.
        -> API 'False (Map Char Text)   -- ^ Map of prompts (as markdown). Part 1 is under \'a\',
                                        --   Part 2 is under \'b\', etc.
    -- | Fetch input
    AInput
        :: Finite 25                -- ^ Day.
        -> API 'True Text

    -- | Submit answer.
    ASubmit
        :: Finite 25                    -- ^ Day.
        -> Char                         -- ^ Part.  \'a\' for part 1, \'b\' for part 2, etc.
        -> String                       -- ^ Answer.  __WARNING__: not escaped or length-limited.
        -> API 'True (Text, SubmitRes)  -- ^ Submission reply (as markdown), and result token

-- | Holds a session key.  @'SessionKey' ''True'@ contains a session key
-- for sure, but @'SessionKey' ''False'@ may or may not contain one.
data SessionKey :: Bool -> Type where
    HasKey :: String -> SessionKey k
    NoKey  :: SessionKey 'False

sessionKeyCookie :: SessionKey 'True -> CurlOption
sessionKeyCookie (HasKey s) = CurlCookie $ printf "session=%s" s

sessionKeyCookieMaybe :: SessionKey k -> Maybe CurlOption
sessionKeyCookieMaybe (HasKey s) = Just (sessionKeyCookie (HasKey s))
sessionKeyCookieMaybe NoKey      = Nothing

-- | Wrap a @'Maybe' 'String'@ (a possible session key) into a 'SessionKey'
-- based on the expectations of a given 'API' command.  If the API
-- command requires a key, 'Nothing' will be returned if no key is given.
-- If the API command doesn't require a key, 'Just' will always be
-- returned.
sessionKey :: API k a -> Maybe String -> Maybe (SessionKey k)
sessionKey = \case
    APrompt{} -> Just . sessionKey_
    AInput{}  -> fmap HasKey
    ASubmit{} -> fmap HasKey

-- | Convert a @'Maybe' 'String'@ (a possible session key) into
-- a @'SessionKey' ''False'@ --- a 'SessionKey' that may or may not contain
-- a key.
sessionKey_ :: Maybe String -> SessionKey 'False
sessionKey_ = maybe NoKey HasKey

apiUrl :: API k a -> FilePath
apiUrl = \case
    APrompt i     -> printf "https://adventofcode.com/2018/day/%d" (dNum i)
    AInput  i     -> printf "https://adventofcode.com/2018/day/%d/input" (dNum i)
    ASubmit i _ _ -> printf "https://adventofcode.com/2018/day/%d/answer" (dNum i)
  where
    dNum = (+ 1) . getFinite

-- | WARNING: does not escape submission answers or limit their length.
apiCurl :: SessionKey k -> API k a -> [CurlOption]
apiCurl sess = \case
    APrompt _       -> maybeToList (sessionKeyCookieMaybe sess)
                    ++ method_GET
    AInput  _       -> sessionKeyCookie sess
                     : method_GET
    ASubmit _ p ans -> sessionKeyCookie sess
                     : CurlPostFields [ printf "level=%d" (partNum p)
                                      , printf "answer=%s" (strip ans)
                                      ]
                     : CurlHttpHeaders ["Content-Type: application/x-www-form-urlencoded"]
                     : method_POST
  where
    partNum p = ord p - ord 'a' + 1

-- | Run an 'API' command with a given 'SessionKey' to produce the result
-- or a list of (lines of) errors.
--
-- __WARNING__: Does not escape submission answers or limit their length,
-- for 'ASubmit'.
runAPI :: SessionKey k -> API k a -> IO (Either [String] a)
runAPI sess a = withCurlDo . runExceptT $ do
    (cc, r) <- liftIO $ curlGetString u (apiCurl sess a)
    case cc of
      CurlOK -> return ()
      _      -> throwError
                    [ "Error contacting Advent of Code server to fetch input"
                    , "Possible invalid session key"
                    , printf "Url: %s" u
                    , printf "Server response: %s" r
                    ]
    liftEither . processAPI a $ r
  where
    u = apiUrl a

processAPI :: API k a -> String -> Either [String] a
processAPI = \case
    APrompt{} -> runErrors
               . traverse eitherToErrors
               . M.fromList
               . zip ['a'..]
               . processHTML True
    AInput{}  -> Right . T.pack
    ASubmit{} -> \rHtml -> do
      out <- join
           . maybeToEither ["No response found"]
           . listToMaybe
           . processHTML False
           $ rHtml
      pure (out, parseSubmitRes out)

processHTML :: Bool -> String -> [Either [String] T.Text]
processHTML pretty html = runExceptT $ do
    article <- lift $ html ^.. packed . H.html
                             . H.allNamed (only "article")
                             . to (set H.name "body")
                             . to H.NodeElement
    let articleText = unpacked . packed . H.html # article
    liftEither . first ((:[]) . show) . P.runPure $ do
      p <- P.readHtml (P.def { P.readerExtensions = exts })
              articleText
      writer (P.def { P.writerExtensions = exts }) p
  where
    writer
      | pretty    = P.writeMarkdown
      | otherwise = P.writePlain
    exts = P.disableExtension P.Ext_header_attributes
         . P.disableExtension P.Ext_smart
         $ P.pandocExtensions

-- | Parse a (markdown or plain text) submission response into
-- a 'SubmitRes'.
parseSubmitRes :: T.Text -> SubmitRes
parseSubmitRes t
    | "the right answer!"        `T.isInfixOf` t = SubCorrect $ findRank t
    | "not the right answer."    `T.isInfixOf` t = SubIncorrect
    | "an answer too recently;"  `T.isInfixOf` t = SubWait
    | "solving the right level." `T.isInfixOf` t = SubInvalid
    | otherwise                                  = SubUnknown

findRank :: Text -> Maybe Int
findRank = go . T.words . T.map onlyAlphaNum . T.toLower
  where
    go ("rank":n:_) = readMaybe $ T.unpack n
    go (_     :ws ) = go ws
    go []           = Nothing
    onlyAlphaNum c
      | isAlphaNum c = c
      | otherwise    = ' '

-- | Pretty-print a 'SubmitRes'
showSubmitRes :: SubmitRes -> String
showSubmitRes = \case
    SubCorrect Nothing  -> "Correct"
    SubCorrect (Just r) -> printf "Correct (Rank %d)" r
    SubIncorrect        -> "Incorrect"
    SubWait             -> "Wait"
    SubInvalid          -> "Invalid"
    SubUnknown          -> "Unknown"
