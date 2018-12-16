-- |
-- Module      : AOC.Challenge.Day16
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day16 (
    day16a
  , day16b
  , trialParser
  ) where

import           AOC.Common                 (eitherToMaybe)
import           AOC.Solver                 ((:~>)(..))
import           Control.Lens               ((^.), (.~), enum)
import           Control.Monad              ((<=<))
import           Control.Monad.Combinators  (between, sepBy1, sepEndBy1)
import           Control.Monad.State        (evalStateT, modify, gets, lift)
import           Data.Bits                  ((.&.), (.|.))
import           Data.Finite                (Finite, packFinite)
import           Data.Foldable              (toList, foldl')
import           Data.Function              ((&))
import           Data.Map                   (Map)
import           Data.Maybe                 (listToMaybe)
import           Data.Set                   (Set)
import           Data.Vector.Sized          (Vector)
import           Data.Void                  (Void)
import           GHC.TypeNats               (KnownNat)
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Vector.Sized          as V
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as P

type Reg = Vector 4 Int

data Instr a = I { _iOp  :: a
                 , _iInA :: Finite 4
                 , _iInB :: Finite 4
                 , _iOut :: Finite 4
                 }
  deriving (Show, Functor)

data Trial = T { _tBefore :: Reg
               , _tInstr  :: Instr (Finite 16)
               , _tAfter  :: Reg
               }
  deriving Show

data OpCode = OAddR | OAddI
            | OMulR | OMulI
            | OBanR | OBanI
            | OBorR | OBorI
            | OSetR | OSetI
            | OGtIR | OGtRI | OGtRR
            | OEqIR | OEqRI | OEqRR
  deriving (Show, Eq, Ord, Enum, Bounded)

runOp :: Instr OpCode -> Reg -> Reg
runOp I{..} = case _iOp of
    OAddR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  +  r ^. V.ix _iInB
    OAddI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  +  fromIntegral _iInB
    OMulR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  *  r ^. V.ix _iInB
    OMulI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  *  fromIntegral _iInB
    OBanR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .&. r ^. V.ix _iInB
    OBanI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .&. fromIntegral _iInB
    OBorR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .|. r ^. V.ix _iInB
    OBorI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .|. fromIntegral _iInB
    OSetR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA
    OSetI -> \r -> r & V.ix _iOut .~                     fromIntegral _iInA
    OGtIR -> \r -> r & V.ix _iOut . enum .~ (fromIntegral _iInA  > r ^. V.ix _iInB   )
    OGtRI -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA     > fromIntegral _iInB)
    OGtRR -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA     > r ^. V.ix _iInB   )
    OEqIR -> \r -> r & V.ix _iOut . enum .~ (fromIntegral _iInA == r ^. V.ix _iInB   )
    OEqRI -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA    == fromIntegral _iInB)
    OEqRR -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA    == r ^. V.ix _iInB   )

plausible :: Trial -> Set OpCode
plausible T{..} = S.fromDistinctAscList . filter tryTrial $ [OAddR ..]
  where
    tryTrial :: OpCode -> Bool
    tryTrial o = runOp (_tInstr { _iOp = o }) _tBefore == _tAfter

day16a :: [Trial] :~> Int
day16a = MkSol
    { sParse = eitherToMaybe . P.parse (trialParser `sepEndBy1` P.newline) ""
    , sShow  = show
    , sSolve = Just . length . filter ((>= 3) . S.size . plausible)
    }

-- | Our search for a unique configuration of op codes.
fromClues :: Map (Finite 16) (Set OpCode) -> Maybe (Vector 16 OpCode)
fromClues m = listToMaybe . flip evalStateT S.empty . V.generateM $ \i -> do
    Just poss <- pure $ M.lookup i m
    unseen    <- gets (poss `S.difference`)
    pick      <- lift $ toList unseen
    modify $ S.insert pick
    pure pick

day16b :: ([Trial], [Instr (Finite 16)]) :~> Int
day16b = MkSol
    { sParse = eitherToMaybe . P.parse
         ((,) <$> (trialParser `sepEndBy1` P.newline) <* P.some P.newline
              <*> (instrParser `sepEndBy1` P.newline)
         ) ""
    , sShow  = show
    , sSolve = \(ts, is) -> do
        opMap <- fromClues . M.fromListWith S.intersection
               $ [ (_iOp (_tInstr t), plausible t)
                 | t <- ts
                 ]
        pure . V.head
             . foldl' (step opMap) (V.replicate 0)
             $ is
    }
  where
    step opMap r i = runOp i' r
      where
        i' = (opMap `V.index`) <$> i





-- ---------
-- | Parsing
-- ---------

type Parser = P.Parsec Void String

trialParser :: Parser Trial
trialParser = T <$> (P.string "Before: " `between` P.newline) (parseVec P.decimal)
                <*> instrParser <* P.newline
                <*> (P.string "After:  " `between` P.newline) (parseVec P.decimal)
  where
    parseVec = maybe (fail "list has bad size") pure . V.fromList <=< parseList
    parseList d = (P.char '[' `between` P.char ']') $
      d `sepBy1` P.try (P.char ',' *> P.space1)

instrParser :: Parser (Instr (Finite 16))
instrParser = I <$> parseFinite <* P.char ' '
                <*> parseFinite <* P.char ' '
                <*> parseFinite <* P.char ' '
                <*> parseFinite
  where
    parseFinite :: KnownNat n => Parser (Finite n)
    parseFinite = maybe (fail "number out of range") pure . packFinite =<< P.decimal
