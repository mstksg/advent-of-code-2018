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
    API(..), SessionKey(..), SubmitRes(..)
  , sessionKey
  , runAPI
  ) where

import           AOC2018.Util
import           Control.Applicative.Lift
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Char
import           Data.Finite
import           Data.Kind
import           Data.Map                   (Map)
import           Data.Maybe
import           Data.Text                  (Text)
import           Data.Text.Lens
import           Network.Curl
import           Text.Printf
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Text.Pandoc                as P
import qualified Text.Taggy                 as H
import qualified Text.Taggy.Lens            as H

-- | The result of a submission.
data SubmitRes = SubCorrect
               | SubWrong
               | SubWait
               | SubInvalid
               | SubUnknown
  deriving Show

-- | An API command.  An @'API' k a@ an AoC API request that returns
-- results of type @a@; if @k@ is ''True', it requires a session key.  If
-- @k@ is ''False', it does not.
data API :: Bool -> Type -> Type where
    -- | Fetch prompts
    APrompt :: Finite 25 -> API 'False (Map Char Text)
    -- | Fetch input
    AInput :: Finite 25  -> API 'True Text
    -- | Submit answer
    ASubmit :: Finite 25 -> Char -> String -> API 'True (Text, SubmitRes)

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
    APrompt{} -> Just . maybe NoKey HasKey
    AInput{}  -> fmap HasKey
    ASubmit{} -> fmap HasKey

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
      _      -> throwE [ "Error contacting Advent of Code server to fetch input"
                       , "Possible invalid session key"
                       , printf "Url: %s" u
                       , printf "Server response: %s" r
                       ]
    either throwE pure . processAPI a $ r
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
    either (throwE . (:[]) . show) pure . P.runPure $ do
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

parseSubmitRes :: T.Text -> SubmitRes
parseSubmitRes t
    | "the right answer!"        `T.isInfixOf` t = SubCorrect
    | "not the right answer."    `T.isInfixOf` t = SubWrong
    | "an answer too recently;"  `T.isInfixOf` t = SubWait
    | "solving the right level." `T.isInfixOf` t = SubInvalid
    | otherwise                                  = SubUnknown
