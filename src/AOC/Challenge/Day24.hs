{-# LANGUAGE OverloadedStrings        #-}
{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day24
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day24  where

-- module AOC.Challenge.Day24 (
--     day24a
--   , day24b
--   ) where

import           AOC.Prelude
import           Control.Lens
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Map                               as M
import qualified Text.Megaparsec                        as P
import qualified Text.Megaparsec.Char                   as P
import qualified Text.Megaparsec.Char.Lexer as P hiding (space)

data Resist = RImmune | RWeak
  deriving (Show, Eq, Ord)

type Resistance = Map String Resist

data Grp = G { _gHP         :: Int
             , _gResist     :: Resistance
             , _gAtk        :: Int
             , _gAtkType    :: String
             , _gInitiative :: Down Int
             }
  deriving (Show, Eq, Ord)

makeLenses ''Grp

type Arena = (Map Grp Int, Map Grp Int)

effPower :: Grp -> Int -> Int
effPower g n = _gAtk g * n

stab :: Grp -> Grp -> Int
stab g1 g2 = case M.lookup (_gAtkType g1) (_gResist g2) of
    Nothing      -> 1
    Just RImmune -> 0
    Just RWeak   -> 2

getDown :: Down a -> a
getDown (Down x) = x

selectTargets
    :: Map Grp Int      -- ^ attackers
    -> Map Grp Int      -- ^ enemies
    -> Map Grp Grp      -- ^ targets
selectTargets atk ene = catMaybes . M.mapKeys (view _3) . flip evalState ene . iforM queue $ \(Down p, _,_) g -> do
    targ <- gets $ fmap (fst . maximumBy (comparing snd))
                 . NE.nonEmpty
                 . filter ((> 0) . view (_2 . _1))
                 -- . map (\(h, n) -> (h, ((stab g h * p) `div` _gHP h, effPower h n, getDown (_gInitiative h))))
                 . map (\(h, n) -> (h, (stab g h * p, effPower h n, getDown (_gInitiative h))))
                 . M.toList
    mapM_ (modify . M.delete) targ
    pure targ
  where
    queue :: Map (Down Int, Down Int, Grp) Grp
    queue = M.fromList . map (\(g, n) -> ((Down $ effPower g n, _gInitiative g, g), g)) $ M.toList atk

-- type Arena' = (Map (Bool, Grp) Int)

data Team = TImm | TInf
  deriving (Show, Eq, Ord)

teamEither :: Team -> a -> Either a a
teamEither TImm = Left
teamEither TInf = Right

makeAttacks
    :: Map Grp Grp
    -> Arena
    -> Arena
makeAttacks targs a = go (mergeArena a, M.empty)
  where
    go :: (Map (Down Int) (Team, Grp, Int), Map Grp (Either Int Int)) -> Arena
    go (queue, finished) = case M.minView queue of
      Nothing      -> M.mapEither id finished
      Just ((t,g,n),queue') -> case M.lookup g targs of
        Nothing   -> go (queue', M.insert g (teamEither t n) finished)
        Just targ -> case find ((== targ) . view (_2 . _2)) (M.toList queue') of
          Nothing -> case M.lookup targ finished of
            Nothing -> go (queue', M.insert g (teamEither t n) finished)
            Just (either id id->m)  ->
              let totDamg   = stab g targ * n * _gAtk g
                  newM      = m - (totDamg `div` _gHP targ)
                  finished'
                    | newM > 0  = finished & ix targ . eitherItem .~ newM
                    | otherwise = M.delete targ finished
              in  go (queue', M.insert g (teamEither t n) finished')
          Just (i, (_,_,m)) ->
              let totDamg   = stab g targ * n * _gAtk g
                  newM      = m - (totDamg `div` _gHP targ)
                  queue''
                    | newM > 0  = queue' & ix i . _3 .~ newM
                    | otherwise = M.delete i queue'
              in  go (queue'', M.insert g (teamEither t n) finished)
    castTeam :: Team -> Grp -> Int -> (Down Int, (Team, Grp, Int))
    castTeam t g n = (_gInitiative g, (t, g, n))
    mergeArena :: Arena -> Map (Down Int) (Team, Grp, Int)
    mergeArena (M.toList->t1, M.toList->t2) = M.fromList $ map (uncurry (castTeam TImm)) t1
                                                        ++ map (uncurry (castTeam TInf)) t2

eitherItem :: Lens' (Either a a) a
eitherItem f (Left x) = Left <$> f x
eitherItem f (Right x) = Right <$> f x

fightBattle :: Arena -> Either Arena (Team, Map Grp Int)
-- fightBattle ((\a -> trace (traceArena a) a)->(t1,t2))
fightBattle a@(t1, t2)
    | a' == a    = Left a
    | M.null t1' = Right (TInf, t2')
    | M.null t2' = Right (TImm, t1')
    | otherwise  = fightBattle (t1', t2')
  where
    targs = selectTargets t1 t2 <> selectTargets t2 t1
    a'@(t1',t2') = makeAttacks targs (t1, t2)

day24a :: _ :~> _
day24a = MkSol
    { sParse = P.parseMaybe parse24
    , sShow  = show
    , sSolve = fmap (sum . snd) . eitherToMaybe . fightBattle
    }

day24b :: _ :~> _
day24b = MkSol
    { sParse = P.parseMaybe parse24
    , sShow  = show
    , sSolve = \a ->
        let goodEnough (traceShowId->i) = case fightBattle (boosted i a) of
              Right (TImm, b) -> Just (sum b)
              _               -> Nothing
        in  exponentialFindMin goodEnough 1
    }

boosted :: Int -> Arena -> Arena
boosted i a = a & _1 %~ M.mapKeys (over gAtk (+ i))

traceArena :: Arena -> String
traceArena (t1, t2) = unlines . concat $
    [ ["Immune System:"]
    , flip M.foldMapWithKey t1 $ \g n -> [printf "Group %d/%d/%d with %d units" (_gHP g) (_gAtk g) (getDown $ _gInitiative g) n]
    , ["Infection:"]
    , flip M.foldMapWithKey t2 $ \g n -> [printf "Group %d/%d/%d with %d units" (_gHP g) (_gAtk g) (getDown $ _gInitiative g) n]
    ]

-- Immune System:
-- 84 units each with 9798 hit points (immune to bludgeoning) with an attack that does 1151 fire damage at initiative 9
-- 255 units each with 9756 hit points (weak to cold, radiation) with an attack that does 382 slashing damage at initiative 17
-- 4943 units each with 6022 hit points (weak to bludgeoning) with an attack that does 11 bludgeoning damage at initiative 4
-- 305 units each with 3683 hit points (weak to bludgeoning, slashing) with an attack that does 107 cold damage at initiative 5
-- 1724 units each with 6584 hit points (weak to radiation) with an attack that does 30 cold damage at initiative 6
-- 2758 units each with 5199 hit points (immune to slashing, bludgeoning, cold; weak to fire) with an attack that does 18 bludgeoning damage at initiative 15
-- 643 units each with 9928 hit points (immune to fire; weak to slashing, bludgeoning) with an attack that does 149 fire damage at initiative 14
-- 219 units each with 8810 hit points with an attack that does 368 cold damage at initiative 3
-- 9826 units each with 10288 hit points (weak to bludgeoning; immune to cold) with an attack that does 8 cold damage at initiative 18
-- 2417 units each with 9613 hit points (weak to fire, cold) with an attack that does 36 cold damage at initiative 19

type Parser_ = P.Parsec Void String

parse24 :: Parser_ (Map Grp Int, Map Grp Int)
parse24 = (,) <$> ("Immune System:" *> P.space *> teamParser <* P.space)
              <*> ("Infection:" *> P.space *> teamParser)

teamParser :: Parser_ (Map Grp Int)
teamParser = M.fromList <$> (P.try groupParser `P.sepEndBy1` P.newline)

groupParser :: Parser_ (Grp, Int)
groupParser = do
    n <- P.decimal
    P.skipMany (P.satisfy (not . isDigit))
    _gHP <- P.decimal <* P.space
    "hit points" <* P.space
    _gResist <- fmap fold . P.optional . P.try $ (P.char '(' `P.between` P.char ')') resistanceParser
    P.skipMany (P.satisfy (not . isDigit))
    _gAtk <- P.decimal <* P.space
    _gAtkType <- P.some (P.satisfy isLetter)
    P.skipMany (P.satisfy (not . isDigit))
    _gInitiative <- Down <$> P.decimal
    pure (G{..}, n)

resistanceParser :: Parser_ Resistance
resistanceParser = M.unions <$> (resistSpec `P.sepBy1` (P.char ';' *> P.space))
  where
    res = (RImmune <$ P.try "immune")
      <|> (RWeak   <$ P.try "weak")
    resistSpec = do
      r <- res <* P.space
      "to" <* P.space
      ts <- P.some (P.satisfy isLetter) `P.sepBy1` (P.char ',' *> P.space)
      pure . M.fromList $ (,r) <$> ts

-- Immune System:
-- 84 units each with 9798 hit points (immune to bludgeoning) with an attack that does 1151 fire damage at initiative 9
-- 255 units each with 9756 hit points (weak to cold, radiation) with an attack that does 382 slashing damage at initiative 17
-- 4943 units each with 6022 hit points (weak to bludgeoning) with an attack that does 11 bludgeoning damage at initiative 4
-- 305 units each with 3683 hit points (weak to bludgeoning, slashing) with an attack that does 107 cold damage at initiative 5
-- 1724 units each with 6584 hit points (weak to radiation) with an attack that does 30 cold damage at initiative 6
-- 2758 units each with 5199 hit points (immune to slashing, bludgeoning, cold; weak to fire) with an attack that does 18 bludgeoning damage at initiative 15
-- 643 units each with 9928 hit points (immune to fire; weak to slashing, bludgeoning) with an attack that does 149 fire damage at initiative 14
-- 219 units each with 8810 hit points with an attack that does 368 cold damage at initiative 3
-- 9826 units each with 10288 hit points (weak to bludgeoning; immune to cold) with an attack that does 8 cold damage at initiative 18
-- 2417 units each with 9613 hit points (weak to fire, cold) with an attack that does 36 cold damage at initiative 19

-- Infection:
-- 1379 units each with 46709 hit points with an attack that does 66 slashing damage at initiative 16
-- 1766 units each with 15378 hit points (weak to bludgeoning) with an attack that does 12 radiation damage at initiative 10
-- 7691 units each with 33066 hit points (weak to bludgeoning) with an attack that does 7 slashing damage at initiative 12
-- 6941 units each with 43373 hit points (weak to cold) with an attack that does 12 fire damage at initiative 7
-- 5526 units each with 28081 hit points (weak to fire, slashing) with an attack that does 7 bludgeoning damage at initiative 11
-- 5844 units each with 41829 hit points with an attack that does 11 bludgeoning damage at initiative 20
-- 370 units each with 25050 hit points (immune to radiation; weak to fire) with an attack that does 120 radiation damage at initiative 2
-- 164 units each with 42669 hit points with an attack that does 481 fire damage at initiative 13
-- 3956 units each with 30426 hit points (weak to radiation) with an attack that does 13 cold damage at initiative 8
-- 2816 units each with 35467 hit points (immune to slashing, radiation, fire; weak to cold) with an attack that does 24 slashing damage at initiative 1
