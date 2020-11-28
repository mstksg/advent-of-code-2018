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

module AOC.Challenge.Day15 (
    day15a
  , day15b
  ) where

import           AOC.Common            (boundingBox, ScanPoint(..), asciiGrid, cardinalNeighbs, mannDist, minimumVal, minimumValBy, floodFill)
import           AOC.Common.Search     (aStar, exponentialFindMin)
import           AOC.Solver            ((:~>)(..))
import           Control.Lens          (makeLenses, ifoldMapOf, (.~), (-~))
import           Control.Monad         (guard)
import           Data.Coerce           (coerce)
import           Data.Function         ((&))
import           Data.Functor.Foldable (Fix, cata, ana, hylo)
import           Data.List             (intercalate)
import           Data.Map              (Map)
import           Data.Ord              (comparing)
import           Data.Semigroup        (First(..), Min(..))
import           Data.Set              (Set)
import           Linear                (V2(..))
import           Text.Printf           (printf)
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Set.NonEmpty     as NES

data EType = EGob | EElf
  deriving (Show, Eq, Ord, Enum, Bounded)

data Entity = E { _eType :: EType
                , _eHP   :: Int
                , _eAtk  :: Int
                }
  deriving (Show, Eq)

makeLenses ''Entity

type World = Set ScanPoint
type Entities = Map ScanPoint Entity

neighbs :: ScanPoint -> Set ScanPoint
neighbs = S.fromList . coerce cardinalNeighbs

type Path = [ScanPoint]

-- | Yes, it's actual literal A*.
actualLiteralAStar
    :: World
    -> ScanPoint
    -> ScanPoint
    -> Maybe Path
actualLiteralAStar w p0 dest = snd <$>
      aStar (mannDist (_getSP dest) . _getSP)
            (M.fromSet (const 1) . (`S.intersection` w) . neighbs)
            p0
            (== dest)

stepTo
    :: Set ScanPoint      -- ^ legal points
    -> ScanPoint
    -> ScanPoint
    -> ScanPoint
stepTo w x dest = maybe (error "stepTo") (snd . getMin)
                . foldMap (\n -> Min . (,n) . length <$> actualLiteralAStar w' n dest)
                . (`S.intersection` w')
                $ neighbs x
  where
    w' = S.delete x w

stepEntity
    :: World
    -> Entities
    -> ScanPoint
    -> Entity
    -> Maybe ScanPoint
stepEntity w es sp e
    | sp `M.member` candidates = Just sp    -- we already here
    | otherwise                = stepTo w' sp <$> destination
  where
    candidates = M.fromSet ( maybe (error "paintBucket") length
                           . actualLiteralAStar fullRange sp
                           )
               . (`S.intersection` fullRange)
               . foldMap neighbs
               . M.keysSet
               . M.filter ((/= _eType e) . _eType)
               $ es
    destination = fst <$> minimumVal candidates
    w'          = w `S.difference` M.keysSet es
    fullRange   = floodFill ((`S.intersection` w') . neighbs)
                            (S.singleton sp)

makeAttack
    :: World
    -> Entities
    -> ScanPoint
    -> Entity
    -> Maybe ScanPoint
makeAttack w es sp e = fst <$> minimumValBy (comparing _eHP) candidates
  where
    candidates = M.delete sp
               . (`M.restrictKeys` w)
               . (`M.restrictKeys` neighbs sp)
               . M.filter ((/= _eType e) . _eType)
               $ es

data BattleLog a = BLTurn  (Maybe EType) a    -- whether or not there was a kill
                 | BLRound a
                 | BLOver  Int      -- sum of remaining HPs
  deriving (Show, Eq, Functor)

damage :: Int
       -> Maybe Entity
       -> (Maybe (First EType), Maybe Entity)  -- if was killed, and new entity
damage _ Nothing = (Nothing, Nothing)
damage d (Just e)
    | _eHP e > d = (Nothing                , Just (e & eHP -~ d))
    | otherwise  = (Just (First (_eType e)), Nothing            )

stepBattle
    :: World
    -> (Entities, Entities)
    -> BattleLog (Entities, Entities)
stepBattle w (!waiting, !done) = case M.minViewWithKey waiting of
    Nothing                      -> BLRound (done, M.empty)
    Just ((p, toMove), waiting') ->
      let allEnts = waiting' <> done
      in  case stepEntity w allEnts p toMove of
            Nothing ->
              let (allFriends, allEnemies) = flip M.mapEither (waiting <> done) $ \e ->
                    if _eType toMove == _eType e
                      then Left (_eHP e)
                      else Right ()
              in  if M.null allEnemies
                    then BLOver $ sum allFriends
                    else BLTurn Nothing (waiting', M.insert p toMove done)
            Just p' -> case makeAttack w allEnts p' toMove of
              Nothing -> BLTurn Nothing (waiting', M.insert p' toMove done)
              Just toAtk ->
                let (killed, (waiting'', done')) =
                      attackBoth (_eAtk toMove) toAtk waiting'
                in  BLTurn (getFirst <$> killed) (waiting'', M.insert p' toMove done')
  where
    attackBoth d k wt = (,) <$> M.alterF (damage d) k wt
                            <*> M.alterF (damage d) k done

getOutcome :: BattleLog (Int, Int) -> (Int, Int)
getOutcome (BLTurn _ !x)       = x
getOutcome (BLRound  (!n, !s)) = (n + 1, s)
getOutcome (BLOver   !s)       = (0, s)

day15a :: (World, Entities) :~> Int
day15a = MkSol
    { sParse = Just . parseWorld
    , sShow  = show
    , sSolve = \(w, e) -> Just
                        . uncurry (*)
                        . hylo getOutcome (stepBattle w)
                        $ (e, M.empty)
    }

totalVictory :: BattleLog Bool -> Bool
totalVictory (BLTurn  (Just EElf) _ ) = False
totalVictory (BLTurn  _           !k) = k
totalVictory (BLRound             !k) = k
totalVictory (BLOver  _             ) = True

day15b :: (World, Entities) :~> Int
day15b = MkSol
    { sParse = Just . parseWorld
    , sShow  = show
    , sSolve = \(w, es) ->
        let goodEnough i = blog <$ guard (cata totalVictory blog)
              where
                blog :: Fix BattleLog
                blog = ana (stepBattle w) (powerUp i es, M.empty)
        in  uncurry (*) . cata getOutcome <$> exponentialFindMin goodEnough 4
    }
  where
    powerUp :: Int -> Entities -> Entities
    powerUp i = fmap $ \e ->
      case _eType e of
        EElf -> e & eAtk .~ i
        EGob -> e







parseWorld :: String -> (World, Entities)
parseWorld = ifoldMapOf asciiGrid $ \(SP->p) -> \case
    '.' -> (S.singleton p, mempty)
    'G' -> (S.singleton p, M.singleton p (E EGob 200 3))
    'E' -> (S.singleton p, M.singleton p (E EElf 200 3))
    _   -> mempty

_displayWorld :: World -> Entities -> String
_displayWorld w es = unlines
    [ row ++ " " ++ intercalate ", " rEs
    | y <- [yMin - 1 .. yMax + 1]
    , let (row, rEs) = makeRow y
    ]
  where
    V2 xMin yMin `V2` V2 xMax yMax
        = boundingBox . NES.unsafeFromSet . S.map _getSP $ w
    makeRow y = flip foldMap [xMin - 1 .. xMax + 1] $ \x ->
      let p = SP (V2 x y)
          inWorld = p `S.member` w
      in  case M.lookup p es of
            Nothing
              | inWorld   -> (".", [])
              | otherwise -> ("#", [])
            Just E{..} -> ( [entChar _eType]
                          , [printf "%c(%d)" (entChar _eType) _eHP]
                          )
    entChar EGob = 'G'
    entChar EElf = 'E'

_unused :: ()
_unused = (eType .~) `seq` ()
