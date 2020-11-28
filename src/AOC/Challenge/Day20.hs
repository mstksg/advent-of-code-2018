-- |
-- Module      : AOC.Challenge.Day20
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day20 (
    day20a
  , day20b
  ) where

import           AOC.Common          (Point, cardinalNeighbs, TokStream, parseTokStreamT_)
import           AOC.Solver          ((:~>)(..))
import           Control.Monad       (guard)
import           Control.Monad.State (State, evalState, put, get)
import           Data.Maybe          (mapMaybe)
import           Data.Set            (Set)
import           Data.Void
import           Linear              (V2(..))
import qualified Data.Set            as S
import qualified Text.Megaparsec     as P

data Edge = E Point Point
  deriving (Show, Eq, Ord)

mkEdge :: Point -> Point -> Edge
mkEdge x y
  | x <= y    = E x y
  | otherwise = E y x

-- | Parse a stream of 'RegTok'.
type Parser_ = P.ParsecT Void (TokStream RegTok) (State Point)

data Dir = DN | DE | DS | DW
  deriving (Eq, Show, Ord, Enum, Bounded)

data RegTok = RTStart
            | RTDir Dir
            | RTRParen
            | RTOr
            | RTLParen
            | RTEnd
  deriving (Show, Eq, Ord)

tok :: RegTok -> Parser_ ()
tok t = P.try $ guard . (== t) =<< P.anySingle

-- | From a stream of 'RegTok', parse a set of all edges.
buildEdges :: Parser_ (Set Edge)
buildEdges = (tok RTStart `P.between` tok RTEnd) anySteps
  where
    anySteps = fmap S.unions . P.many $
            P.try basicStep P.<|> branchStep
    branchStep = (tok RTRParen `P.between` tok RTLParen) $ do
      initPos <- get
      fmap S.unions . (`P.sepBy` tok RTOr) $ do
        put initPos
        anySteps
    basicStep = do
      currPos <- get
      RTDir d <- P.anySingle
      let newPos = currPos + case d of
            DN -> V2   0 (-1)
            DE -> V2   1   0
            DS -> V2   0   1
            DW -> V2 (-1)  0
      put newPos
      S.insert (mkEdge currPos newPos) <$> anySteps

farthestRoom :: Set Edge -> Int
farthestRoom es = go 0 S.empty (V2 0 0)
  where
    go :: Int -> Set Point -> Point -> Int
    go n seen p
        | null allNeighbs = n
        | otherwise       = maximum $ go (n + 1) (S.insert p seen) <$> allNeighbs
      where
        allNeighbs = filter ((`S.member` es) . mkEdge p)
                   . filter (`S.notMember` seen)
                   $ cardinalNeighbs p

day20a :: [RegTok] :~> Int
day20a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

roomDistances :: Set Edge -> [Int]
roomDistances es = go 0 S.empty (V2 0 0)
  where
    go :: Int -> Set Point -> Point -> [Int]
    go n seen p = (n :) $
        concatMap (go (n + 1) (S.insert p seen)) allNeighbs
      where
        allNeighbs = filter ((`S.member` es) . mkEdge p)
                   . filter (`S.notMember` seen)
                   $ cardinalNeighbs p

day20b :: [RegTok] :~> Int
day20b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }


parseToks :: String -> [RegTok]
parseToks = mapMaybe $ \case
    '^' -> Just RTStart
    'N' -> Just $ RTDir DN
    'E' -> Just $ RTDir DE
    'W' -> Just $ RTDir DW
    'S' -> Just $ RTDir DS
    '|' -> Just RTOr
    '(' -> Just RTRParen
    ')' -> Just RTLParen
    '$' -> Just RTEnd
    _   -> Nothing

