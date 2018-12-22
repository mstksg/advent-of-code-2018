
module AOC.Common.Elfcode (
    Mem, IMem
  , ECProg
  , Instr(..)
  , OpCode(..)
  , runOp
  , execOp
  , runECProg
  , evalECProg
  , execECProg
  , traceECProg
  , traceECProg_
  -- * Optimizers
  , optimizeEC
  , Peephole
  , peep
  , currPeepPos
  -- * Parsing
  , parseElfcode
  , elfcodeParser
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Primitive
import           Control.Monad.ST.Lazy
import           Control.Monad.Writer
import           Data.Bits
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec.Char.Lexer        (decimal)
import qualified Data.Vector                       as UV
import qualified Data.Vector.Unboxed.Mutable.Sized as MV
import qualified Data.Vector.Unboxed.Sized         as V
import qualified Text.Megaparsec                   as P
import qualified Text.Megaparsec.Char              as P
import qualified Text.Parsec                       as Pa

type Mem s = MV.MVector 6 s Int
type IMem  = V.Vector 6 Int

type ECProg = UV.Vector Instr

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
            | ONoOp
            | OOutR | OOutI
  deriving (Show, Eq, Ord, Enum, Bounded)

execOp :: (PrimMonad m, PrimState m ~ s) => Instr -> Mem s -> m (Maybe Int)
execOp I{..} = case _iOp of
    OAddR -> rrOp (+)
    OAddI -> riOp (+)
    OMulR -> rrOp (*)
    OMulI -> riOp (*)
    OBanR -> rrOp (.&.)
    OBanI -> riOp (.&.)
    OBorR -> rrOp (.|.)
    OBorI -> riOp (.|.)
    OSetR -> riOp const
    OSetI -> \m -> Nothing <$ MV.write m _iOut _iInA
    OGtIR -> irOp $ \x y -> if x  > y then 1 else 0
    OGtRI -> riOp $ \x y -> if x  > y then 1 else 0
    OGtRR -> rrOp $ \x y -> if x  > y then 1 else 0
    OEqIR -> irOp $ \x y -> if x == y then 1 else 0
    OEqRI -> riOp $ \x y -> if x == y then 1 else 0
    OEqRR -> rrOp $ \x y -> if x == y then 1 else 0
    ODivR -> rrOp div
    ODivI -> riOp div
    OModR -> rrOp mod
    ONoOp -> \_ -> pure Nothing
    OOutR -> \m -> Just <$> MV.read m _iOut
    OOutI -> \_ -> pure $ Just (fromIntegral _iOut)
  where
    rrOp f m = do
      x <- MV.read m (fromIntegral _iInA)
      y <- MV.read m (fromIntegral _iInB)
      Nothing <$ MV.write m _iOut (f x y)
    riOp f m = (Nothing <$) . MV.write m _iOut . (`f` _iInB) =<< MV.read m (fromIntegral _iInA)
    irOp f m = (Nothing <$) . MV.write m _iOut . f _iInA     =<< MV.read m (fromIntegral _iInB)

runOp :: Instr -> IMem -> (Maybe Int, IMem)
runOp i v = runST $ strictToLazyST $ do
    mv <- V.thaw v
    res <- execOp i mv
    (res,) <$> V.freeze mv

runECProg
    :: Finite 6
    -> ECProg
    -> IMem
    -> ([Int], IMem)
runECProg iPtr p v = runST $ do
    mv <- strictToLazyST $ V.thaw v
    let go = do
          i <- strictToLazyST $ MV.read mv iPtr
          case p UV.!? i of
            Nothing    -> pure []
            Just instr -> do
              out <- strictToLazyST $ execOp instr mv
              strictToLazyST $ MV.modify mv (+1) iPtr
              (maybeToList out ++) <$> go
    res <- go
    (res,) <$> strictToLazyST (V.freeze mv)

execECProg
    :: Finite 6
    -> ECProg
    -> IMem
    -> IMem
execECProg iPtr p = snd . runECProg iPtr p

evalECProg
    :: Finite 6
    -> ECProg
    -> IMem
    -> [Int]
evalECProg iPtr p = fst . runECProg iPtr p

traceECProg
    :: Finite 6
    -> ECProg
    -> IMem
    -> [(Maybe Int, IMem)]
traceECProg iPtr p = go
  where
    go !v = case p UV.!? V.index v iPtr of
      Nothing    -> []
      Just instr ->
        let o@(_, v') = runOp instr v
        in  o : go (v' & V.ix iPtr +~ 1)

traceECProg_
    :: Finite 6
    -> ECProg
    -> IMem
    -> [IMem]
traceECProg_ iPtr p = map snd . traceECProg iPtr p




type Peephole = Pa.Parsec [Instr] ()

optimizeEC
    :: [Peephole [Instr]]
    -> ECProg
    -> ECProg
optimizeEC os = UV.fromList . go . toList
  where
    go m0 = case Pa.parse (optimizers os) "" m0 of
      Left _   -> m0
      Right m1
        | m0 == m1  -> m0
        | otherwise -> go m1

currPeepPos :: Peephole Int
currPeepPos = subtract 1 . Pa.sourceLine <$> Pa.getPosition

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


optimizers
    :: [Peephole [Instr]]
    -> Peephole [Instr]
optimizers os = concat <$> P.many ( Pa.choice (Pa.try <$> os)
                                <|> ((:[]) <$> peep Nothing Nothing Nothing)
                                  )




parseElfcode :: String -> Maybe (Finite 6, ECProg)
parseElfcode = P.parseMaybe elfcodeParser

type Parser = P.Parsec Void String

elfcodeParser :: Parser (Finite 6, ECProg)
elfcodeParser = (,) <$> (P.string "#ip " `P.between` P.newline) parseFinite
                    <*> (UV.fromList . catMaybes <$> (lineParser `P.sepEndBy1` P.space))
  where
    lineParser = P.try (Just <$> instrParser)
             <|> Nothing <$ (P.char '#' *> P.many (P.noneOf "\n"))

instrParser :: Parser Instr
instrParser = I <$> parseOpCode <* P.space1
                <*> decimal     <* P.space1
                <*> decimal     <* P.space1
                <*> parseFinite <* P.skipMany (P.satisfy (/= '\n'))
  where
    parseOpCode = P.choice . flip map [OAddR ..] $ \o ->
        o <$ P.try (P.string (map toLower . drop 1 . show $ o))

parseFinite :: Parser (Finite 6)
parseFinite = maybe (fail "number out of range") pure . packFinite =<< decimal
