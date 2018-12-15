{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day15
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day15 where

-- module AOC.Challenge.Day15 (
--     day15a
--   , day15b
--   ) where

import           AOC.Prelude hiding    (inRange)
import           Control.Lens
import           Data.Functor.Foldable
import qualified Data.Map              as M
import qualified Data.Set              as S

data EType = EGob | EElf
  deriving (Show, Eq, Ord, Enum, Bounded)

data Entity = E { _eType :: EType, _eHP :: Int }
  deriving (Show, Eq)

makeLenses ''Entity

type Point = V2 Int

type World = Set ScanPoint
type Entities = Map ScanPoint Entity

-- | It's 'Point', but with a newtype wrapper so we have an 'Ord' that
-- sorts by y first, then x
newtype ScanPoint = SP { _gSP :: Point }
  deriving (Eq, Show, Num)

instance Ord ScanPoint where
    compare = comparing (view _y . _gSP)
           <> comparing (view _x . _gSP)

dist :: ScanPoint -> ScanPoint -> Int
dist (SP x) (SP y) = sum . abs $ x - y

neighbs :: ScanPoint -> Set ScanPoint
neighbs p = S.fromList . map ((+ p) . SP) $ [V2 (-1) 0, V2 0 (-1), V2 1 0, V2 0 1]

inRange :: Set ScanPoint -> Set ScanPoint
inRange = foldMap neighbs

pickNearest :: ScanPoint -> Set ScanPoint -> Maybe ScanPoint
pickNearest x = fmap snd . S.lookupMin . S.map (\y -> (dist x y, y))

type Path = [ScanPoint]

actualLiteralAStar
    :: World
    -> ScanPoint
    -> ScanPoint
    -> Maybe Path
actualLiteralAStar w p0 dest = go (M.singleton (costOf []) (S.singleton []))
  where
    go :: Map Int (Set Path) -> Maybe Path
    go !queue = M.minView queue >>= \(!candidates, !queue') ->
         (reverse <$> S.lookupMin (S.filter isGoal candidates))
     <|> (go . M.unionsWith (<>) $ queue' : foldMap (map addBack . expand) candidates)
    expand :: Path -> [Path]
    expand ps = map (:ps)
              . toList
              . (`S.difference` S.fromList ps)
              . (`S.intersection` w)
              . neighbs
              . fromMaybe p0
              . listToMaybe $ ps
    addBack :: Path -> Map Int (Set Path)
    addBack p = M.singleton (costOf p) (S.singleton p)
    costOf :: Path -> Int
    costOf xs = dist (fromMaybe p0 (listToMaybe xs)) dest
              + length xs
    isGoal :: Path -> Bool
    isGoal = (== Just dest) . listToMaybe


-- | TODO: implement actual literal A star
stepTo
    :: World
    -> Entities
    -> ScanPoint
    -> ScanPoint
    -> Maybe ScanPoint
stepTo w es x dest = fmap snd
                   . listToMaybe
                   . catMaybes
                   . toList
                   . S.map (\n -> (,n) . length <$> actualLiteralAStar w' n dest)
                   . (`S.intersection` w')
                   $ neighbs x
  where
    w' = S.delete x $ w `S.difference` M.keysSet es

stepEntity
    :: World
    -> Entities
    -> ScanPoint
    -> Entity
    -> Maybe ScanPoint
stepEntity w es sp e
    | sp `S.member` candidates = Just sp    -- we already here
    | otherwise                = stepTo w es sp =<< destination
  where
    -- Possible positions to move to
    candidates = (`S.difference` M.keysSet es)
               . (`S.intersection` w)
               . inRange
               . M.keysSet
               . M.filter ((/= _eType e) . _eType)
               $ es
    destination = pickNearest sp candidates

makeAttack
    :: World
    -> Entities
    -> ScanPoint
    -> Entity
    -> Maybe ScanPoint
makeAttack w es sp e = fmap snd
                     . S.lookupMin
                     . M.foldMapWithKey (\p e' -> S.singleton (_eHP e', p))
                     $ candidates
  where
    candidates = M.delete sp
               . (`M.restrictKeys` w)
               . (`M.restrictKeys` neighbs sp)
               . M.filter ((/= _eType e) . _eType)
               $ es

data BattleLog a = BLTurn  (ScanPoint, ScanPoint, Entity) Entities a
                 | BLRound Entities a
                 | BLOver  Entities Int      -- sum of remaining HPs
  deriving (Show, Eq, Functor)

damage :: Maybe Entity
       -> Maybe Entity
damage Nothing = Nothing
damage (Just e)
    | _eHP e > 3 = Just (e & eHP -~ 3)
    | otherwise  = Nothing

stepBattle
    :: World
    -> (Entities, Entities)
    -> BattleLog (Entities, Entities)
stepBattle w (waiting, done) = case M.minViewWithKey waiting of
    Nothing                      -> trace "round is done" $ BLRound done (done, M.empty)
    Just ((p, toMove), waiting') ->
      let allEnts = waiting' <> done
      in  case stepEntity w allEnts p toMove of
            Nothing ->
              let (allFriends, allEnemies) = flip M.mapEither (waiting <> done) $ \e ->
                    if _eType toMove == _eType e
                      then Left (_eHP e)
                      else Right ()
              in  if M.null allEnemies
                    then BLOver (waiting <> done) $ sum allFriends
                    else BLTurn (p, p, toMove) (waiting <> done) (waiting', M.insert p toMove done)
            Just p' -> case makeAttack w allEnts p' toMove of
              Nothing -> BLTurn (p, p', toMove) allEnts (waiting', M.insert p' toMove done)
              Just toAtk ->
                BLTurn  (p, p', toMove) (M.insert p' toMove $ M.alter damage toAtk allEnts)
                        ( M.alter damage toAtk waiting'
                        , M.insert p' toMove $ M.alter damage toAtk done
                        )

getOutcome :: BattleLog (Int, Int) -> (Int, Int)
getOutcome (BLTurn _ _ !x) = x
getOutcome (BLRound _ !x) = first (+1) x
getOutcome (BLOver e !i) = traceShow e (0, i)

traceOutcome = \case
    BLTurn p e es -> es
    BLRound e es -> Left e : es
    BLOver e _ -> [Right e]

day15a :: (World, Entities) :~> _
day15a = MkSol
    { sParse = Just . parseWorld
    -- , sShow  = unlines . map show . take 100 . zip [1..]
    -- , sSolve = \(w, e) -> Just . hylo traceOutcome (stepBattle w) $ (e, M.empty)
    , sShow  = show
    , sSolve = \(w, e) -> Just . uncurry (*) . traceShowId . hylo getOutcome (stepBattle w) $ (e, M.empty)
    }

day15b :: _ :~> _
day15b = MkSol
    { sParse = Just
    , sShow  = id
    , sSolve = Just
    }


parseWorld :: String -> (World, Entities)
parseWorld = ifoldMapOf (lined <.> folded) (uncurry classify)
  where
    classify y x = \case
        '.' -> (S.singleton p, mempty)
        'G' -> (S.singleton p, M.singleton p (E EGob 200))
        'E' -> (S.singleton p, M.singleton p (E EElf 200))
        _   -> mempty
      where
        p = SP $ V2 x y
