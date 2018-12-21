{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

module AOC.Challenge.Day21 (
    day21a
  , day21b
  ) where

import           AOC.Prelude
import           Control.Lens
import           Control.Monad.ST.Lazy
import           Control.Monad.ST.Unsafe
import           Control.Monad.Writer
import           Data.Bits
import qualified Data.Set                                      as S
import qualified Data.Vector                                   as UV
import qualified Data.Vector.Unboxed.Mutable.Sized             as MV
import qualified Data.Vector.Unboxed.Sized                     as V
import qualified Text.Megaparsec                               as P
import qualified Text.Megaparsec.Char                          as P
import qualified Text.Megaparsec.Char.Lexer        as P hiding (space)
import qualified Text.Parsec                                   as Pa

type Mem s = MV.MVector 6 s Int

type Program = UV.Vector Instr

data Instr = I { _iOp    :: OpCode
               , _iInA   :: Int
               , _iInB   :: Int
               , _iOut   :: Finite 6
               }
  deriving (Show, Eq, Ord)

data OpCode = OAddR | OAddI
            | OMulR | OMulI
            | OBanR | OBanI
            | OBorR | OBorI
            | OSetR | OSetI
            | OGtIR | OGtRI | OGtRR
            | OEqIR | OEqRI | OEqRR
            | ODivR | ODivI
            | OModR
            | ONoOp | OTrce
  deriving (Show, Eq, Ord, Enum, Bounded)

runOp :: (MonadWriter [Int] m, PrimMonad m, PrimState m ~ s) => Instr -> Mem s -> m ()
runOp I{..} = case _iOp of
    OAddR -> rrOp (+)
    OAddI -> riOp (+)
    OMulR -> rrOp (*)
    OMulI -> riOp (*)
    OBanR -> rrOp (.&.)
    OBanI -> riOp (.&.)
    OBorR -> rrOp (.|.)
    OBorI -> riOp (.|.)
    OSetR -> riOp const
    OSetI -> \m -> MV.write m _iOut _iInA
    OGtIR -> irOp $ \x y -> if x  > y then 1 else 0
    OGtRI -> riOp $ \x y -> if x  > y then 1 else 0
    OGtRR -> rrOp $ \x y -> if x  > y then 1 else 0
    OEqIR -> irOp $ \x y -> if x == y then 1 else 0
    OEqRI -> riOp $ \x y -> if x == y then 1 else 0
    OEqRR -> rrOp $ \x y -> if x == y then 1 else 0
    ODivR -> rrOp div
    ODivI -> riOp div
    OModR -> rrOp mod
    ONoOp -> \_ -> pure ()
    OTrce -> \m -> do
        res <- MV.read m (fromIntegral _iOut)
        tell [res]
  where
    rrOp f m = do
      x <- MV.read m (fromIntegral _iInA)
      y <- MV.read m (fromIntegral _iInB)
      MV.write m _iOut (f x y)
    riOp f m = MV.write m _iOut . (`f` _iInB) =<< MV.read m (fromIntegral _iInA)
    irOp f m = MV.write m _iOut . f _iInA     =<< MV.read m (fromIntegral _iInB)

runProgram
    :: Finite 6
    -> Program
    -> V.Vector 6 Int
    -> [Int]
runProgram iPtr p v = runST $ do
    mv <- strictToLazyST $ V.thaw v
    let go = do
          i <- strictToLazyST $ MV.read mv iPtr
          case p ^? ix i of
            Nothing    -> pure []
            Just instr -> do
              out <- strictToLazyST $ execWriterT (runOp instr mv)
              strictToLazyST $ MV.modify mv (+1) iPtr
              (out ++) <$> go
    go

day21a :: _ :~> _
day21a = MkSol
    { sParse = P.parseMaybe progParser
    , sShow  = show
    , sSolve = \(i, p) -> listToMaybe
                        . runProgram i p
                        $ V.replicate 0
    }

day21b :: _ :~> _
day21b = MkSol
    { sParse = P.parseMaybe progParser
    , sShow  = show
    , sSolve = \(i, p) -> listToMaybe . reverse . uniqRun
                        . runProgram i p
                        $ V.replicate 0
    }


uniqRun :: [Int] -> [Int]
uniqRun = go S.empty
  where
    go _    []     = []
    go seen (x:xs)
      | x `S.member` seen = []
      | otherwise         = x : go (S.insert x seen) xs



type Parser = P.Parsec Void String

progParser :: Parser (Finite 6, Program)
progParser = (,) <$> (P.string "#ip " `P.between` P.newline) parseFinite
                 <*> (UV.fromList . catMaybes <$> (lineParser `P.sepEndBy1` P.space))
  where
    lineParser = P.try (Just <$> instrParser)
             <|> Nothing <$ (P.char '#' *> P.many (P.noneOf "\n"))

instrParser :: Parser Instr
instrParser = I <$> parseOpCode <* P.space1
                <*> P.decimal   <* P.space1
                <*> P.decimal   <* P.space1
                <*> parseFinite <* P.skipMany (P.satisfy (/= '\n'))
  where
    parseOpCode = P.choice . flip map [OAddR ..] $ \o ->
        o <$ P.try (P.string (map toLower . drop 1 . show $ o))

parseFinite :: Parser (Finite 6)
parseFinite = maybe (fail "number out of range") pure . packFinite =<< P.decimal
