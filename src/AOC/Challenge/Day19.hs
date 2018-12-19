{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

module AOC.Challenge.Day19 (
    day19a
  , day19b
  ) where

import           AOC.Prelude
import           Control.Lens hiding        (uncons)
import           Data.Bitraversable
import           Data.Bits
import           Data.Vector.Unboxed.Sized  (Vector)
import qualified Data.Map                   as M
import qualified Data.Vector.Unboxed.Sized  as V
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as P

type Reg = Vector 6 Int

type Memory = Map Int Instr

-- data ProgState = PS { _psInstr :: Int
--                     , _psReg   :: Reg
--                     , _psMem   :: Memory
--                     }

-- makeLenses ''ProgState

data Instr = I { _iOp  :: OpCode
               , _iInA :: Finite 6
               , _iInB :: Int
               , _iOut :: Finite 6
               }
  deriving (Show, Eq, Ord)

data OpCode = OAddR | OAddI
            | OMulR | OMulI
            | OBanR | OBanI
            | OBorR | OBorI
            | OSetR | OSetI
            | OGtIR | OGtRI | OGtRR
            | OEqIR | OEqRI | OEqRR
  deriving (Show, Eq, Ord, Enum, Bounded)

runOp :: Instr -> Reg -> Reg
runOp I{..} = case _iOp of
    OAddR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  +  r ^. V.ix (fromIntegral _iInB)
    OAddI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  +  _iInB
    OMulR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  *  r ^. V.ix (fromIntegral _iInB)
    OMulI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  *  _iInB
    OBanR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .&. r ^. V.ix (fromIntegral _iInB)
    OBanI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .&. _iInB
    OBorR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .|. r ^. V.ix (fromIntegral _iInB)
    OBorI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .|. _iInB
    OSetR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA
    OSetI -> \r -> r & V.ix _iOut .~                     fromIntegral _iInA
    OGtIR -> \r -> r & V.ix _iOut . enum .~ (fromIntegral _iInA  > r ^. V.ix (fromIntegral _iInB)   )
    OGtRI -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA     > _iInB)
    OGtRR -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA     > r ^. V.ix (fromIntegral _iInB)   )
    OEqIR -> \r -> r & V.ix _iOut . enum .~ (fromIntegral _iInA == r ^. V.ix (fromIntegral _iInB)   )
    OEqRI -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA    == _iInB)
    OEqRR -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA    == r ^. V.ix (fromIntegral _iInB)   )

stepMemory
    :: Finite 6
    -> Memory
    -> Reg
    -> Maybe Reg
stepMemory iPtr mem r0 = M.lookup i mem <&> \instr ->
    runOp instr r0 & V.ix iPtr +~ 1
  where
    i = r0 ^. V.ix iPtr

day19a :: _ :~> _
day19a = MkSol
    { sParse = P.parseMaybe memParser
    , sShow  = show
    , sSolve = \(i, m) -> Just
                        . V.head
                        . last
                        . iterateMaybe (stepMemory i m)
                        $ V.replicate 0
    }

day19b :: _ :~> _
day19b = MkSol
    { sParse = P.parseMaybe memParser
    , sShow  = show
    , sSolve = \(i, m) -> Just
                        . V.head
                        . last
                        -- . last
                        . take 1000
                        . iterateMaybe (stepMemory i m . traceShowId)
                        . set (V.ix 0) 1
                        $ V.replicate 0
    }

-- this just checks if m is a factor of targ
-- aka, if R3 is a factor of R2
-- seti 1 1 1          # [A]: n = 1
-- mulr 3 1 4          # [B]:
-- eqrr 4 2 4          # if (n * m) == targ:
-- addr 4 5 5          #   then: GOTO [C] (add m to output)
-- addi 5 1 5          #   else: GOTO [D]
-- addr 3 0 0          # [C] out += m       -- the thing
-- addi 1 1 1          # [D] n   += 1
-- gtrr 1 2 4          # if (n > targ):
-- addr 5 4 5          #   then: GOTO [E]
-- seti 2 7 5          #   else: GOTO [B]
-- >>
-- if m is a factor of targ:
--   then: out += m
-- goto: e


type Parser = P.Parsec Void String

memParser :: Parser (Finite 6, Memory)
memParser = (,) <$> (P.string "#ip " `P.between` P.newline) parseFinite
                <*> (M.fromList . zip [0..] <$> (instrParser `P.sepEndBy1` P.newline))

instrParser :: Parser Instr
instrParser = I <$> parseOpCode <* P.char ' '
                <*> parseFinite <* P.char ' '
                <*> P.decimal   <* P.char ' '
                <*> parseFinite <* P.skipMany (P.satisfy (/= '\n'))
  where
    parseOpCode = P.choice . flip map [OAddR ..] $ \o ->
        o <$ P.try (P.string (map toLower . drop 1 . show $ o))

parseFinite :: Parser (Finite 6)
parseFinite = maybe (fail "number out of range") pure . packFinite =<< P.decimal
