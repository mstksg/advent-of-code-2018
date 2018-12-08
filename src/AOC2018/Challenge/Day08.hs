{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC2018.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC2018.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC2018.Prelude
import           Control.Lens
import qualified Data.Tree       as Tree
import qualified Text.Parsec     as P

type Parser = P.Parsec [Int] ()

sum1 :: Parser Int
sum1 = do
    numChild <- P.anyToken
    numMeta  <- P.anyToken
    childs   <- replicateM numChild sum1
    metas    <- replicateM numMeta  P.anyToken
    pure $ sum (metas ++ childs)

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
