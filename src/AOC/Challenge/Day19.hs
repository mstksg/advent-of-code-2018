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

import           AOC.Common                 (iterateMaybe)
import           AOC.Solver                 ((:~>)(..))
import           Control.Applicative        ((<|>))
import           Control.Lens               ((^.), (.~), (+~), (^?), (^?!), set, ix, enum)
import           Control.Monad              (mfilter, guard)
import           Data.Bits                  ((.&.), (.|.))
import           Data.Char                  (toLower)
import           Data.Finite                (Finite, packFinite)
import           Data.Foldable              (toList, forM_)
import           Data.Function              ((&))
import           Data.Functor               ((<&>))
import           Data.Void                  (Void)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Vector                as UV
import qualified Data.Vector.Unboxed.Sized  as V
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as P
import qualified Text.Parsec                as Pa

type Reg = V.Vector 6 Int

type Memory = UV.Vector Instr

data Instr = I { _iOp  :: OpCode
               , _iInA :: Int
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
    OAddR -> \r -> r & V.ix _iOut .~ r ^?! ix _iInA  +  r ^?! ix _iInB
    OAddI -> \r -> r & V.ix _iOut .~ r ^?! ix _iInA  +  _iInB
    OMulR -> \r -> r & V.ix _iOut .~ r ^?! ix _iInA  *  r ^?! ix _iInB
    OMulI -> \r -> r & V.ix _iOut .~ r ^?! ix _iInA  *  _iInB
    OBanR -> \r -> r & V.ix _iOut .~ r ^?! ix _iInA .&. r ^?! ix _iInB
    OBanI -> \r -> r & V.ix _iOut .~ r ^?! ix _iInA .&. _iInB
    OBorR -> \r -> r & V.ix _iOut .~ r ^?! ix _iInA .|. r ^?! ix _iInB
    OBorI -> \r -> r & V.ix _iOut .~ r ^?! ix _iInA .|. _iInB
    OSetR -> \r -> r & V.ix _iOut .~ r ^?! ix _iInA
    OSetI -> \r -> r & V.ix _iOut .~          _iInA
    OGtIR -> \r -> r & V.ix _iOut . enum .~ (         _iInA  > r ^?! ix _iInB)
    OGtRI -> \r -> r & V.ix _iOut . enum .~ (r ^?! ix _iInA  >          _iInB)
    OGtRR -> \r -> r & V.ix _iOut . enum .~ (r ^?! ix _iInA  > r ^?! ix _iInB)
    OEqIR -> \r -> r & V.ix _iOut . enum .~ (         _iInA == r ^?! ix _iInB)
    OEqRI -> \r -> r & V.ix _iOut . enum .~ (r ^?! ix _iInA ==          _iInB)
    OEqRR -> \r -> r & V.ix _iOut . enum .~ (r ^?! ix _iInA == r ^?! ix _iInB)
    OModR -> \r -> r & V.ix _iOut .~ r ^?! ix _iInA `mod` r ^?! ix _iInB
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

day19a :: (Finite 6, Memory) :~> Int
day19a = MkSol
    { sParse = P.parseMaybe memParser
    , sShow  = show
    , sSolve = \(i, m) -> fmap (V.head . NE.last) . NE.nonEmpty
                        . iterateMaybe (stepMemory i m)
                        $ V.replicate 0
    }

day19b :: (Finite 6, Memory) :~> Int
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

currPos :: Peephole Int
currPos = subtract 1 . Pa.sourceLine <$> Pa.getPosition

peep
    :: Maybe Int                -- ^ expected A
    -> Maybe Int                -- ^ expected B
    -> Maybe (Finite 6)         -- ^ expected C
    -> Peephole Instr
peep eA eB eC = Pa.tokenPrim show (\p _ _ -> Pa.incSourceLine p 1) $ \i@I{..} -> do
    forM_ eA $ guard . (== _iInA)
    forM_ eB $ guard . (== _iInB)
    forM_ eC $ guard . (== _iOut)
    pure i


addIfIsFactor
    :: Finite 6      -- ^ instruction register
    -> Peephole [Instr]
addIfIsFactor i = do
    a <- currPos
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
    b <- currPos
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

optimize :: Finite 6 -> Peephole [Instr]
optimize i = concat <$> P.many (Pa.try (addIfIsFactor i) <|> ((:[]) <$> peep Nothing Nothing Nothing))





type Parser = P.Parsec Void String

memParser :: Parser (Finite 6, Memory)
memParser = (,) <$> (P.string "#ip " `P.between` P.newline) parseFinite
                <*> (UV.fromList <$> (instrParser `P.sepEndBy1` P.newline))

instrParser :: Parser Instr
instrParser = I <$> parseOpCode <* P.char ' '
                <*> P.decimal   <* P.char ' '
                <*> P.decimal   <* P.char ' '
                <*> parseFinite <* P.skipMany (P.satisfy (/= '\n'))
  where
    parseOpCode = P.choice . flip map [OAddR ..] $ \o ->
        o <$ P.try (P.string (map toLower . drop 1 . show $ o))

parseFinite :: Parser (Finite 6)
parseFinite = maybe (fail "number out of range") pure . packFinite =<< P.decimal
