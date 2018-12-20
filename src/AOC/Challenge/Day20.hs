{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day20 where
-- module AOC.Challenge.Day20 (
--     day20a
--   , day20b
--   , parseTok
--   ) where

import           AOC.Prelude
import qualified Data.Set as S
import qualified Text.Parsec as P

data Edge = E Point Point
  deriving (Show, Eq, Ord)

mkEdge :: Point -> Point -> Edge
mkEdge x y
  | x <= y    = E x y
  | otherwise = E y x

-- | Parse a stream of @('Time', 'Action')@ events
type Parser = P.Parsec [RegTok] Point

data Dir = DN | DE | DS | DW
  deriving (Eq, Show, Ord, Enum, Bounded)

data RegTok = RTStart
            | RTDir Dir
            | RTStartGroup
            | RTOr
            | RTEndGroup
            | RTEnd
  deriving (Show, Eq, Ord)

dirStep = \case
    DN -> V2 0    (-1)
    DE -> V2 1    0
    DS -> V2 0    1
    DW -> V2 (-1) 0

isTok t = P.try $ do
    d <- P.anyToken
    guard $ d == t

-- | From a stream of @('Time', 'Action')@ events, accumulate a map of
-- guards to time cards.
buildEdges :: Parser (Set Edge)
buildEdges = (isTok RTStart `P.between` isTok RTEnd) go
  where
    go = do
      outs <- P.many $ P.try basicStep P.<|> splitStep
      pure (S.unions outs)
    splitStep = (isTok RTStartGroup `P.between` isTok RTEndGroup) $ do
      initPos <- P.getState
      outs <- flip P.sepBy (isTok RTOr) $ do
        P.setState initPos
        go
      pure $ S.unions outs
    basicStep = do
      currPos <- P.getState
      RTDir d <- P.anyToken
      let newPos = currPos + dirStep d
      P.setState newPos
      S.insert (mkEdge currPos newPos) <$> go

day20a :: [RegTok] :~> Int
day20a = MkSol
    { sParse = Just . parseTok
    , sShow  = show
    , sSolve = fmap farthestEdge
             . traceShowId
             . eitherToMaybe
             . P.runParser buildEdges (V2 0 0) ""
    }

farthestEdge :: Set Edge -> Int
farthestEdge es = go 0 S.empty (V2 0 0)
  where
    go :: Int -> Set Point -> Point -> Int
    go n seen p
        | null allNeighbs = n
        | otherwise       = maximum $ go (n + 1) (S.insert p seen) <$> allNeighbs
      where
        allNeighbs = filter ((`S.member` es) . mkEdge p)
                   . filter (`S.notMember` seen)
                   $ neighbs p
      
bigRooms :: Set Edge -> [Int]
bigRooms es = filter (>= 1000) $ go 0 S.empty (V2 0 0)
  where
    go :: Int -> Set Point -> Point -> [Int]
    go n seen p
        | null allNeighbs = [n]
        | otherwise       = n : (go (n + 1) (S.insert p seen) =<< allNeighbs)
      where
        allNeighbs = filter ((`S.member` es) . mkEdge p)
                   . filter (`S.notMember` seen)
                   $ neighbs p

neighbs :: Point -> [Point]
neighbs p = [ p + V2 dx dy
            | dx <- [-1 .. 1]
            , dy <- if dx == 0 then [-1,1] else [-1..1]
            ]


day20b :: [RegTok] :~> Int
day20b = MkSol
    { sParse = Just . parseTok
    , sShow  = show
    , sSolve = fmap (length . bigRooms)
             . traceShowId
             . eitherToMaybe
             . P.runParser buildEdges (V2 0 0) ""
    }


parseTok :: String -> [RegTok]
parseTok = foldMap $ \case
    '^' -> [RTStart]
    'N' -> [RTDir DN]
    'E' -> [RTDir DE]
    'W' -> [RTDir DW]
    'S' -> [RTDir DS]
    '|' -> [RTOr]
    '(' -> [RTStartGroup]
    ')' -> [RTEndGroup]
    '$' -> [RTEnd]
    _   -> []

