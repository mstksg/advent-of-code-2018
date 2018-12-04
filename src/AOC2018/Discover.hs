{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : AOC2018.Discover
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Template Haskell for discovering all named challenges in a given
-- directory.
--

module AOC2018.Discover (
    mkChallengeMap
  , solutionList
  , ChallengeMap
  , ChallengeSpec(..)
  , dayToInt
  , solSpec
  ) where

import           AOC2018.Solver
import           AOC2018.Util
import           Data.Bifunctor
import           Data.Data
import           Data.Finite
import           Data.Map                   (Map)
import           Data.Maybe
import           Data.Traversable
import           Data.Void
import           Language.Haskell.Exts      as E
import           Language.Haskell.Names
import           Language.Haskell.TH        as TH
import           Language.Haskell.TH.Syntax (TExp(..))
import           Prelude
import           System.Directory
import           System.FilePath
import           Text.Printf
import           Text.Read                  (readMaybe)
import qualified Data.Map                   as M
import qualified Hpack.Config               as H
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P

-- | Get a 'ChallengeSpec' from a given reified solution (name).
--
-- @
-- solSpec \'day02a == CS { _csDay = 1, _csPart = 'a' }
-- @
--
solSpec :: TH.Name -> ChallengeSpec
solSpec n = either error id $ do
    (d0, p) <- case nameBase n of
      'd':'a':'y':d1:d2:p:_ -> pure ([d1,d2], p)
      _                     -> Left "Function name doesn't fit naming convention."
    d1 <- subtract 1 <$> maybeToEither "Could not parse day" (readMaybe d0)
    d2 <- maybeToEither "Day out of range" (packFinite d1)
    pure $ CS d2 p

-- | A specification for a specific challenge.  Should consist of a day and
-- a lowercase character.
data ChallengeSpec = CS { _csDay  :: Finite 25
                        , _csPart :: Char
                        }
  deriving (Show, Eq, Ord)

-- | A map of days to parts to solutions.
type ChallengeMap = Map (Finite 25) (Map Char SomeSolution)

type Parser = P.Parsec Void String

-- | Turn a day represented by a @'Finite' 25@ into an integer day (1
-- - 25).
dayToInt :: Finite 25 -> Int
dayToInt = fromIntegral . (+ 1) . getFinite

-- | Template Haskell splice to produce a list of all named solutions in
-- a directory. Expects solutions as function names following the format
-- @dayDDp@, where @DD@ is a two-digit zero-added day, and @p@ is
-- a lower-case letter corresponding to the part of the challenge.
--
-- See 'mkChallengeMap' for a description of usage.
solutionList :: FilePath -> Q (TExp [(Finite 25, (Char, SomeSolution))])
solutionList dir = TExp
                  . ListE
                  . map (unType . specExp)
                <$> runIO (getChallengeSpecs dir)


-- | Meant to be called like:
--
-- @
-- mkChallengeMap $$(solutionList "src\/AOC2018\/Challenge")
-- @
mkChallengeMap :: [(Finite 25, (Char, SomeSolution))] -> ChallengeMap
mkChallengeMap = M.unionsWith M.union
               . map (uncurry M.singleton . second (uncurry M.singleton))


specExp :: ChallengeSpec -> TExp (Finite 25, (Char, SomeSolution))
specExp s@(CS d p) = TExp $ TupE
    [ LitE (IntegerL (getFinite d))
    , TupE
        [ LitE (CharL p)
        , ConE 'MkSomeSol `AppE` VarE (mkName (specName s))
        ]
    ]

specName :: ChallengeSpec -> String
specName (CS d p) = printf "day%02d%c" (dayToInt d) p

getChallengeSpecs
    :: FilePath                 -- ^ directory of modules
    -> IO [ChallengeSpec]       -- ^ all challenge specs found
getChallengeSpecs dir = do
    exts   <- defaultExtensions
    files  <- listDirectory dir
    parsed <- forM files $ \f -> do
      let mode = defaultParseMode { extensions    = exts
                                  , fixities      = Just []
                                  , parseFilename = f
                                  }
      res <- parseFileWithMode mode (dir </> f)
      case res of
        ParseOk x       -> pure x
        ParseFailed l e -> fail $ printf "Failed parsing %s at %s: %s" f (show l) e
    pure $ moduleSolutions parsed

defaultExtensions :: IO [E.Extension]
defaultExtensions = do
    Right (H.DecodeResult{..}) <- H.readPackageConfig H.defaultDecodeOptions
    Just H.Section{..} <- pure $ H.packageLibrary decodeResultPackage
    pure $ parseExtension <$> sectionDefaultExtensions

moduleSolutions :: (Data l, Eq l) => [Module l] -> [ChallengeSpec]
moduleSolutions = (foldMap . foldMap) (maybeToList . isSolution)
                . flip resolve M.empty


isSolution :: Symbol -> Maybe ChallengeSpec
isSolution s = do
    Value _ (Ident _ n) <- pure s
    Right c             <- pure $ P.runParser challengeName "" n
    pure c

challengeName :: Parser ChallengeSpec
challengeName = do
    _    <- P.string "day"
    dStr <- P.many P.numberChar
    dInt <- case readMaybe dStr of
      Just i  -> pure (i - 1)
      Nothing -> fail "Failed parsing integer"
    dFin <- case packFinite dInt of
      Just i  -> pure i
      Nothing -> fail $ "Day not in range: " ++ show dInt
    c    <- P.lowerChar
    pure $ CS dFin c
