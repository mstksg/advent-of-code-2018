{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day24
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day24 (
    day24a
  , day24b
  ) where

import           AOC.Common                 (getDown, eitherToMaybe)
import           AOC.Common.Search          (exponentialFindMin)
import           AOC.Solver                 ((:~>)(..))
import           Control.Lens               (ix, at, view, uses, iforM, (.~), (.=), _1, _2, _3, non)
import           Control.Monad.State        (evalState)
import           Data.Char                  (isDigit, isLetter)
import           Data.Foldable              (fold, forM_, maximumBy)
import           Data.Function              ((&))
import           Data.Map                   (Map)
import           Data.Ord                   (Down(..), comparing)
import           Data.OrdPSQ                (OrdPSQ)
import           Data.Void                  (Void)
import           Data.Witherable            (catMaybes)
import           Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.OrdPSQ                as PSQ
import qualified Data.Set                   as S
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P

data Resist = RImmune | RWeak
  deriving (Show, Eq, Ord)

type Resistance = Map String Resist

data Team = TImm | TInf
  deriving (Show, Eq, Ord)

data Grp = G { _gHP         :: Int
             , _gResist     :: Resistance
             , _gAtk        :: Int
             , _gAtkType    :: String
             , _gInitiative :: Down Int
             , _gTeam       :: Team
             }
  deriving (Show, Eq, Ord)

type Arena = Map Grp Int

effPower :: Grp -> Int -> Int
effPower g n = _gAtk g * n

stab :: Grp -> Grp -> Int
stab g1 g2 = case M.lookup (_gAtkType g1) (_gResist g2) of
    Nothing      -> 1
    Just RImmune -> 0
    Just RWeak   -> 2

selectTargets
    :: Arena
    -> Map Grp Grp      -- ^ targets
selectTargets a = catMaybes . M.mapKeys (view _3) . flip evalState candidates . iforM queue $ \(Down p, _,_) g -> do
    targ <- uses (at (_gTeam g) . non M.empty) $
                   fmap (fst . maximumBy (comparing snd))
                 . NE.nonEmpty
                 . filter ((> 0) . view (_2 . _1))
                 . map (\(h, n) -> (h, (stab g h * p, effPower h n, getDown (_gInitiative h))))
                 . M.toList
    forM_ targ $ \t ->
      ix (_gTeam g) . at t .= Nothing
    pure targ
  where
    queue :: Map (Down Int, Down Int, Grp) Grp
    queue = M.fromList . map (\(g, n) -> ((Down $ effPower g n, _gInitiative g, g), g)) $ M.toList a
    candidates :: Map Team Arena
    candidates = flip M.fromSet (S.fromDistinctAscList [TImm, TInf]) $ \t ->
        M.filterWithKey (\g _ -> _gTeam g /= t) a

makeAttacks
    :: Map Grp Grp
    -> Arena
    -> Arena
makeAttacks targs a = go queue0 M.empty
  where
    go  :: OrdPSQ Grp (Down Int) Int
        -> Map    Grp            Int
        -> Arena
    go queue finished = case PSQ.minView queue of
      Nothing      -> finished
      Just (g,_,n,queue') -> case M.lookup g targs of
        Nothing   -> go queue' (M.insert g n finished)
        Just targ -> case PSQ.lookup targ queue' of
          Nothing -> case M.lookup targ finished of
            Nothing -> go queue' (M.insert g n finished)
            Just m  ->
              let totDamg   = stab g targ * n * _gAtk g
                  newM      = m - (totDamg `div` _gHP targ)
                  finished'
                    | newM > 0  = finished & ix targ .~ newM
                    | otherwise = M.delete targ finished
              in  go queue' (M.insert g n finished')
          Just (_,m) ->
              let totDamg   = stab g targ * n * _gAtk g
                  newM      = m - (totDamg `div` _gHP targ)
                  queue''
                    | newM > 0  = queue' & ix targ .~ newM
                    | otherwise = PSQ.delete targ queue'
              in  go queue'' (M.insert g n finished)
    castTeam :: (Grp, Int) -> (Grp, Down Int, Int)
    castTeam (g, n) = (g, _gInitiative g, n)
    queue0 :: OrdPSQ Grp (Down Int) Int
    queue0 = PSQ.fromList . map castTeam . M.toList $ a

fightBattle :: Arena -> Either Arena (Team, Map Grp Int)
fightBattle a
    | a' == a             = Left a
    | all (== TImm) teams = Right (TImm, a')
    | all (== TInf) teams = Right (TInf, a')
    | otherwise           = fightBattle a'
  where
    a' = makeAttacks (selectTargets a) a
    teams = _gTeam <$> M.keys a'

day24a :: _ :~> _
day24a = MkSol
    { sParse = P.parseMaybe parse24
    , sShow  = show
    , sSolve = fmap (sum . snd) . eitherToMaybe . fightBattle
    }

day24b :: _ :~> _
day24b = MkSol
    { sParse = P.parseMaybe parse24
    , sShow  = show
    , sSolve = \a ->
        let goodEnough i = case fightBattle (boost i a) of
              Right (TImm, b) -> Just (sum b)
              _               -> Nothing
        in  exponentialFindMin goodEnough 1
    }
  where
    boost :: Int -> Arena -> Arena
    boost i = M.mapKeys $ \g -> case _gTeam g of
        TImm -> g { _gAtk = _gAtk g + i }
        TInf -> g



type Parser_ = P.Parsec Void String

parse24 :: Parser_ Arena
parse24 = M.union <$> ("Immune System:" *> P.space *> teamParser TImm <* P.space)
                  <*> ("Infection:"     *> P.space *> teamParser TInf)

teamParser :: Team -> Parser_ Arena
teamParser t = M.fromList <$> (P.try (groupParser t) `P.sepEndBy1` P.newline)

groupParser :: Team -> Parser_ (Grp, Int)
groupParser _gTeam = do
    n <- decimal
    P.skipMany (P.satisfy (not . isDigit))
    _gHP <- decimal <* P.space
    "hit points" <* P.space
    _gResist <- fmap fold . P.optional . P.try $ (P.char '(' `P.between` P.char ')') resistanceParser
    P.skipMany (P.satisfy (not . isDigit))
    _gAtk <- decimal <* P.space
    _gAtkType <- P.some (P.satisfy isLetter)
    P.skipMany (P.satisfy (not . isDigit))
    _gInitiative <- Down <$> decimal
    pure (G{..}, n)

resistanceParser :: Parser_ Resistance
resistanceParser = M.unions <$> (resistSpec `P.sepBy1` (P.char ';' *> P.space))
  where
    res   = (RImmune <$ P.try "immune")
      P.<|> (RWeak   <$ P.try "weak")
    resistSpec = do
      r <- res <* P.space
      "to" <* P.space
      ts <- P.some (P.satisfy isLetter) `P.sepBy1` (P.char ',' *> P.space)
      pure . M.fromList $ (,r) <$> ts
