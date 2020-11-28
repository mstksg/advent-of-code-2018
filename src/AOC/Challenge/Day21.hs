-- |
-- Module      : AOC.Challenge.Day21
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day21 (
    day21a
  , day21b
  ) where

import           AOC.Common.Elfcode        (ECProg, Instr(..), OpCode(..), Peephole, IMem, currPeepPos, peep, traceECProg_, optimizeEC, parseElfcode)
import           AOC.Solver                ((:~>)(..))
import           Data.Finite               (Finite)
import           Data.Maybe                (listToMaybe, mapMaybe)
import qualified Data.Set                  as S
import qualified Data.Vector               as UV
import qualified Data.Vector.Unboxed.Sized as V

-- | All the times where the program checks if something is equal to the
-- value in register zero
zeroChecks
    :: Finite 6
    -> ECProg
    -> [IMem]
    -> [Int]
zeroChecks iPtr prog = mapMaybe checksZero
  where
    checksZero v = prog UV.!? (v `V.index` iPtr) >>= \case
      I OEqIR x 0 _ -> Just x
      I OEqRI 0 x _ -> Just x
      I OEqRR 0 r _ -> Just $ v `V.index` fromIntegral r
      I OEqRR r 0 _ -> Just $ v `V.index` fromIntegral r
      _             -> Nothing

day21a :: (Finite 6, ECProg) :~> Int
day21a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

day21b :: (Finite 6, ECProg) :~> Int
day21b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

uniqRun :: [Int] -> [Int]
uniqRun = go S.empty
  where
    go _    []     = []
    go seen (x:xs)
      | x `S.member` seen = []
      | otherwise         = x : go (S.insert x seen) xs

-- | Peephole optimize a division
division
    :: Finite 6      -- ^ instruction register
    -> Peephole [Instr]
division i = do
    a <- currPeepPos
    I OSetI _ _ z1 <- peep (Just 0)   Nothing   Nothing
    let z1' = fromIntegral z1
    I OAddI _ _ z2 <- peep (Just z1') (Just 1)  Nothing
    let z2' = fromIntegral z2
    n <- peep (Just z2') Nothing (Just z2) >>= \case
      I OMulR _ n _ -> pure $ Left  n
      I OMulI _ n _ -> pure $ Right n
      _             -> fail "No match"
    I OGtRR _ m _  <- peep (Just z2') Nothing   (Just z2)
    I OAddR _ _ _  <- peep (Just z2') (Just i') (Just i )
    I OAddI _ _ _  <- peep (Just i' ) (Just 1 ) (Just i )
    I OSetI q _ _  <- peep Nothing    Nothing   (Just i )
    I OAddI _ _ _  <- peep (Just z1') (Just 1 ) (Just z1)
    I OSetI _ _ _  <- peep (Just a  ) Nothing   (Just i )
    I OSetR _ _ o  <- peep (Just z1') Nothing   Nothing
    c <- currPeepPos
    pure . take (c - a) $
        [ case n of
            Left  r -> I ODivR m r o
            Right x -> I ODivI m x o
        , I OSetI q 0 i
        ] ++ repeat (I ONoOp 0 0 0)
  where
    i' = fromIntegral i
