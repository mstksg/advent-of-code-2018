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

import           AOC.Common    (Point, eitherToMaybe)
import           AOC.Solver    ((:~>)(..))
import           Control.Monad (guard)
import           Data.Maybe    (mapMaybe)
import           Data.Set      (Set)
import           Linear        (V2(..))
import qualified Data.Set      as S
import qualified Text.Parsec   as P

data Edge = E Point Point
  deriving (Show, Eq, Ord)

mkEdge :: Point -> Point -> Edge
mkEdge x y
  | x <= y    = E x y
  | otherwise = E y x

-- | Parse a stream of @('Time', 'Action')@ events
type Parser_ = P.Parsec [RegTok] Point

data Dir = DN | DE | DS | DW
  deriving (Eq, Show, Ord, Enum, Bounded)

data RegTok = RTStart
            | RTDir Dir
            | RTRParen
            | RTOr
            | RTLParen
            | RTEnd
  deriving (Show, Eq, Ord)

isTok :: RegTok -> Parser_ ()
isTok t = P.try $ do
    d <- P.anyToken
    guard $ d == t

-- | From a stream of @('Time', 'Action')@ events, accumulate a map of
-- guards to time cards.
buildEdges :: Parser_ (Set Edge)
buildEdges = (isTok RTStart `P.between` isTok RTEnd) go
  where
    go = fmap S.unions . P.many $
            P.try basicStep P.<|> splitStep
    splitStep = (isTok RTRParen `P.between` isTok RTLParen) $ do
      initPos <- P.getState
      fmap S.unions . flip P.sepBy (isTok RTOr) $ do
        P.setState initPos
        go
    basicStep = do
      currPos <- P.getState
      RTDir d <- P.anyToken
      let newPos = currPos + case d of
            DN -> V2   0 (-1)
            DE -> V2   1   0
            DS -> V2   0   1
            DW -> V2 (-1)  0
      P.setState newPos
      S.insert (mkEdge currPos newPos) <$> go

neighbs :: Point -> [Point]
neighbs p = [ p + V2 dx dy
            | dx <- [-1 .. 1]
            , dy <- if dx == 0 then [-1,1] else [-1..1]
            ]

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
                   $ neighbs p
                   
day20a :: [RegTok] :~> Int
day20a = MkSol
    { sParse = Just . parseTok
    , sShow  = show
    , sSolve = fmap farthestRoom
             . eitherToMaybe
             . P.runParser buildEdges (V2 0 0) ""
    }

roomDistances :: Set Edge -> [Int]
roomDistances es = go 0 S.empty (V2 0 0)
  where
    go :: Int -> Set Point -> Point -> [Int]
    go n seen p = (n :) $
        go (n + 1) (S.insert p seen) =<< allNeighbs
      where
        allNeighbs = filter ((`S.member` es) . mkEdge p)
                   . filter (`S.notMember` seen)
                   $ neighbs p
                   
day20b :: [RegTok] :~> Int
day20b = MkSol
    { sParse = Just . parseTok
    , sShow  = show
    , sSolve = fmap (length . filter (>= 1) . roomDistances)
             . eitherToMaybe
             . P.runParser buildEdges (V2 0 0) ""
    }


parseTok :: String -> [RegTok]
parseTok = mapMaybe $ \case
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

