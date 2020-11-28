-- |
-- Module      : AOC.Challenge.Day19
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day19 (
    day19a
  , day19b
  ) where

import           AOC.Common.Elfcode
import           AOC.Solver                ((:~>)(..))
import           Control.Lens              (set)
import           Control.Monad             (mfilter)
import           Data.Finite               (Finite)
import qualified Data.Vector.Unboxed.Sized as V

day19a :: (Finite 6, ECProg) :~> Int
day19a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

day19b :: (Finite 6, ECProg) :~> Int
day19b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }



addIfIsFactor
    :: Finite 6      -- ^ instruction register
    -> Peephole [Instr]
addIfIsFactor i = do
    a <- currPeepPos
    let a' = fromIntegral a
    I OSetI _ _ n <- peep (Just 1 ) Nothing   Nothing
    let n' = fromIntegral n
    I OMulR m _ z <- peep Nothing   (Just n') Nothing
    let z' = fromIntegral z
    I OEqRR _ t _ <- peep (Just z') Nothing   (Just z )
    I OAddR _ _ _ <- peep (Just z') (Just i') (Just i )
    I OAddI _ _ _ <- peep (Just i') (Just 1 ) (Just i )
    I OAddR _ o _ <- mfilter (\I{..} -> fromIntegral _iInB == _iOut)
                   $ peep (Just m ) Nothing   Nothing
    I OAddI _ _ _ <- peep (Just n') (Just 1 ) (Just n )
    I OGtRR _ _ _ <- peep (Just n') (Just t ) (Just z )
    I OAddR _ _ _ <- peep (Just i') (Just z') (Just i )
    I OSetI _ _ _ <- peep (Just a') Nothing   (Just i )
    b <- currPeepPos
    let t' = fromIntegral t
        o' = fromIntegral o
    pure . take (b - a) $
        [ I OModR t' m  z           -- store (t `mod` m) to z
        , I OEqRI z' 0  z           -- is z zero?
        , I OAddR z' i' i           -- if yes, jump down
        , I OAddI i' 1  i           -- otherwise, jump down even more
        , I OAddR m  o  o'          -- increment the thing
        ] ++ repeat (I ONoOp 0 0 0)
  where
    i' = fromIntegral i
