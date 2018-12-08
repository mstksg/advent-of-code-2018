-- |
-- Module      : AOC2018.Challenge.Day08
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC2018.Solver" for the types used in this module!

module AOC2018.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC2018.Solver ((:~>)(..))
import           AOC2018.Util   (eitherToMaybe)
import           Control.Lens   ((^?), ix)
import           Control.Monad  (replicateM)
import           Data.Maybe     (mapMaybe)
import           Text.Read      (readMaybe)
import qualified Text.Parsec    as P

type Parser = P.Parsec [Int] ()

sum1 :: Parser Int
sum1 = do
    numChild <- P.anyToken
    numMeta  <- P.anyToken
    childs   <- sum <$> replicateM numChild sum1
    metas    <- sum <$> replicateM numMeta  P.anyToken
    pure $ childs + metas

day08a :: [Int] :~> Int
day08a = MkSol
    { sParse = traverse readMaybe . words
    , sShow  = show
    , sSolve = eitherToMaybe . P.parse sum1 ""
    }

sum2 :: Parser Int
sum2 = do
    numChild <- P.anyToken
    numMeta  <- P.anyToken
    childs   <- replicateM numChild sum2
    metas    <- replicateM numMeta  P.anyToken
    pure $ if null childs
      then sum metas
      else sum . mapMaybe (\i -> childs ^? ix (i - 1)) $ metas

day08b :: [Int] :~> Int
day08b = MkSol
    { sParse = traverse readMaybe . words
    , sShow  = show
    , sSolve = eitherToMaybe . P.parse sum2 ""
    }
