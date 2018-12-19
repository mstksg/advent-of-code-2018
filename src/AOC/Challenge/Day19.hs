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
import           Control.Lens
import           Data.Bitraversable
import           Data.Bits
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Vector                as UV
import qualified Data.Vector.Unboxed.Sized  as V
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as P
import qualified Text.Parsec                as Pa

type Reg = V.Vector 6 Int

type Memory = UV.Vector Instr

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
            | OModR | ONoOp
  deriving (Show, Eq, Ord, Enum, Bounded)

runOp :: Instr -> Reg -> Reg
runOp I{..} = case _iOp of
    OAddR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  +  r ^?! ix _iInB
    OAddI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  +  _iInB
    OMulR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  *  r ^?! ix _iInB
    OMulI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  *  _iInB
    OBanR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .&. r ^?! ix _iInB
    OBanI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .&. _iInB
    OBorR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .|. r ^?! ix _iInB
    OBorI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .|. _iInB
    OSetR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA
    OSetI -> \r -> r & V.ix _iOut .~                     fromIntegral _iInA
    OGtIR -> \r -> r & V.ix _iOut . enum .~ (fromIntegral _iInA  > r ^?! ix _iInB)
    OGtRI -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA     > _iInB)
    OGtRR -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA     > r ^?! ix _iInB)
    OEqIR -> \r -> r & V.ix _iOut . enum .~ (fromIntegral _iInA == r ^?! ix _iInB)
    OEqRI -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA    == _iInB)
    OEqRR -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA    == r ^?! ix _iInB)
    OModR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA `mod` r ^?! ix _iInB
    ONoOp -> id

stepMemory
    :: Finite 6
    -> Memory
    -> Reg
    -> Maybe Reg
stepMemory iPtr mem r0 = (mem ^? ix i) <&> \instr ->
    runOp instr r0 & V.ix iPtr +~ 1
  where
    i = r0 ^. V.ix iPtr

day19a :: _ :~> _
day19a = MkSol
    { sParse = P.parseMaybe memParser
    , sShow  = show
    , sSolve = \(i, m) -> fmap (V.head . NE.last) . NE.nonEmpty
                        . iterateMaybe (stepMemory i m)
                        $ V.replicate 0
    }

day19b :: _ :~> _
day19b = MkSol
    { sParse = P.parseMaybe memParser
    , sShow  = show
    , sSolve = \(i, m) -> fmap (V.head . NE.last) . NE.nonEmpty
                        . iterateMaybe (stepMemory i (runOptimizer i m))
                        . set (V.ix 0) 1
                        $ V.replicate 0
    }




type Peephole = Pa.Parsec [Instr] ()

runOptimizer :: Finite 6 -> Memory -> Memory
runOptimizer i m0 = checkSize
                  . either (const (errorWithoutStackTrace "optimization failure"))
                           UV.fromList
                  . Pa.parse (optimize i) ""
                  $ toList m0
  where
    checkSize m
      | length m == length m0 = m
      | otherwise             = errorWithoutStackTrace "optimization should preserve length"

peep :: Peephole Instr
peep = Pa.tokenPrim show (\p _ _ -> Pa.incSourceLine p 1) Just
currPos :: Peephole Int
currPos = subtract 1 . Pa.sourceLine <$> Pa.getPosition

addIfIsFactor
    :: Finite 6      -- ^ instruction register
    -> Peephole [Instr]
addIfIsFactor i = do
    a <- currPos
    I OSetI 1             _              n             <- peep
    I OMulR m             ((==n)
                    .fromIntegral->True) z             <- peep         -- this can be cleaner with GADT OpCode
    I OEqRR ((==z)->True) t              ((==z)->True) <- peep
    I OAddR ((==z)->True) ((==i')->True) ((==i)->True) <- peep
    I OAddI ((==i)->True) 1              ((==i)->True) <- peep
    I OAddR ((==m)->True) o              _             <- mfilter (\I{..} -> fromIntegral _iInB == _iOut) peep
    I OAddI ((==n)->True) 1              ((==n)->True) <- peep
    I OGtRR ((==n)->True) ((==t )->True) ((==z)->True) <- peep
    I OAddR ((==i)->True) ((==z)
                    .fromIntegral->True) ((==i)->True) <- peep
    I OSetI _             _              ((==i)->True) <- mfilter (\I{..} -> fromIntegral _iInA == a) peep
    b <- currPos
    let t' = fromIntegral t
        o' = fromIntegral o
        m' = fromIntegral m
    pure . take (b - a) $
        [ I OModR t' m' z           -- store (t `mod` m) to z
        , I OEqRI z  0  z           -- is z zero?
        , I OAddR z  i' i           -- if yes, jump down
        , I OAddI i  1  i           -- otherwise, jump down even more
        , I OAddR m  o  o'          -- increment the thing
        ] ++ repeat (I ONoOp 0 0 0)
  where
    i' = fromIntegral i

optimize :: Finite 6 -> Peephole [Instr]
optimize i = concat <$> many (Pa.try (addIfIsFactor i) <|> ((:[]) <$> peep))





type Parser = P.Parsec Void String

memParser :: Parser (Finite 6, Memory)
memParser = (,) <$> (P.string "#ip " `P.between` P.newline) parseFinite
                <*> (UV.fromList <$> (instrParser `P.sepEndBy1` P.newline))

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
