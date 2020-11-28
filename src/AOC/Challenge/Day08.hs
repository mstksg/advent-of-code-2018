-- |
-- Module      : AOC.Challenge.Day08
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Common      (TokStream, parseTokStream_)
import           AOC.Solver      ((:~>)(..))
import           Control.Lens    ((^?), ix)
import           Control.Monad   (replicateM)
import           Data.Maybe      (mapMaybe)
import           Data.Void       (Void)
import           Text.Read       (readMaybe)
import qualified Text.Megaparsec as P

type Parser = P.Parsec Void (TokStream Int)

sum1 :: Parser Int
sum1 = do
    numChild <- P.anySingle
    numMeta  <- P.anySingle
    childs   <- sum <$> replicateM numChild sum1
    metas    <- sum <$> replicateM numMeta  P.anySingle
    pure $ childs + metas

day08a :: [Int] :~> Int
day08a = MkSol
    { sParse = traverse readMaybe . words
    , sShow  = show
    , sSolve = parseTokStream_ sum1
    }

sum2 :: Parser Int
sum2 = do
    numChild <- P.anySingle
    numMeta  <- P.anySingle
    childs   <- replicateM numChild sum2
    metas    <- replicateM numMeta  P.anySingle
    pure $ if null childs
      then sum metas
      else sum . mapMaybe (\i -> childs ^? ix (i - 1)) $ metas

day08b :: [Int] :~> Int
day08b = MkSol
    { sParse = traverse readMaybe . words
    , sShow  = show
    , sSolve = parseTokStream_ sum2
    }
