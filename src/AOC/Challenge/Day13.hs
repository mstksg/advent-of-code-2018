-- |
-- Module      : AOC.Challenge.Day13
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day13 (
    day13a
  , day13b
  ) where

import           AOC.Solver            ((:~>)(..))
import           Control.Lens          (view, makeLenses, (^.), (%~), (+~), (<.>), ifoldMapOf, folded, lined)
import           Data.Function         ((&))
import           Data.Functor.Foldable (hylo)
import           Data.Map              (Map)
import           Data.Ord              (comparing)
import           Linear                (V2(..), _x, _y)
import qualified Data.Map              as M

type Point = V2 Int

data Track = TStraight    -- ^ go straight
           | TTurnNW      -- ^ a forward-slash mirror @/@
           | TTurnNE      -- ^ a backwards-slash mirror @\\@
           | TInter       -- ^ a four-way intersection
  deriving (Eq, Show, Ord)

data Dir = DN | DE | DS | DW
  deriving (Eq, Show, Ord, Enum, Bounded)

data Cart = C { _cDir   :: Dir
              , _cTurns :: Int
              }
  deriving (Eq, Show)

makeLenses ''Cart

-- | It's 'Point', but with a newtype wrapper so we have an 'Ord' that
-- sorts by y first, then x
newtype ScanPoint = SP { _getSP :: Point }
  deriving (Eq, Show, Num)

instance Ord ScanPoint where
    compare = comparing (view _y . _getSP)
           <> comparing (view _x . _getSP)

type World = Map Point     Track
type Carts = Map ScanPoint Cart

-- | Step a single cart through the world.
stepCart :: World -> ScanPoint -> Cart -> (ScanPoint, Cart)
stepCart w (SP p) c = (SP p',) $ case w M.! p' of
    TTurnNW   -> c & cDir   %~ \case DN -> DE; DE -> DN; DS -> DW; DW -> DS
    TTurnNE   -> c & cDir   %~ \case DN -> DW; DW -> DN; DS -> DE; DE -> DS
    TInter    -> c & cDir   %~ turnWith (c ^. cTurns)
                   & cTurns +~ 1
    TStraight -> c
  where
    p' = p + case c ^. cDir of
      DN -> V2 0    (-1)
      DE -> V2 1    0
      DS -> V2 0    1
      DW -> V2 (-1) 0
    turnWith i = case i `mod` 3 of
      0 -> turnLeft
      1 -> id
      _ -> turnLeft . turnLeft . turnLeft
    turnLeft DN = DW
    turnLeft DE = DN
    turnLeft DS = DE
    turnLeft DW = DS

-- | One of the ways a single step of the simulation can go.
data CartLog a = CLCrash Point a      -- ^ A crash, at a given point
               | CLTick        a      -- ^ No crashes, just a normal timestep
               | CLDone  Point        -- ^ Only one car left, at a given point
  deriving (Show, Functor)

-- | Given a (waiting, done) queue, emit a 'CartLog' event with an updated
-- (waiting, done) queue.
stepCarts
    :: World
    -> (Carts, Carts)
    -> CartLog (Carts, Carts)
stepCarts w = uncurry go
  where
    go :: Carts -> Carts -> CartLog (Carts, Carts)
    go waiting done = case M.minViewWithKey waiting of
      Nothing -> case M.minViewWithKey done of
        Just ((SP lastPos, _), M.null->True) -> CLDone lastPos
        _                                    -> CLTick (done, M.empty)
      Just (uncurry (stepCart w) -> (p, c), waiting') ->
        case M.lookup p (waiting' <> done) of
          Nothing -> CLTick (waiting', M.insert p c done)
          Just _  -> CLCrash (_getSP p) (M.delete p waiting', M.delete p done)

-- | Given a folding function, simulate on events emitted by 'stepCarts'.
simulateWith
    :: (CartLog a -> a)
    -> World
    -> Carts
    -> a
simulateWith f w c = (f `hylo` stepCarts w) (c, M.empty)

day13a :: (World, Carts) :~> Point
day13a = MkSol
    { sParse = Just . parseWorld
    , sShow  = \(V2 x y) -> show x ++ "," ++ show y
    , sSolve = Just . uncurry (simulateWith firstCrash)
    }
  where
    firstCrash (CLCrash p _) = p
    firstCrash (CLTick    p) = p
    firstCrash (CLDone  p  ) = p


day13b :: (World, Carts) :~> Point
day13b = MkSol
    { sParse = Just . parseWorld
    , sShow  = \(V2 x y) -> show x ++ "," ++ show y
    , sSolve = Just . uncurry (simulateWith lastPoint)
    }
  where
    lastPoint (CLCrash _ p) = p
    lastPoint (CLTick    p) = p
    lastPoint (CLDone  p  ) = p

parseWorld :: String -> (World, Carts)
parseWorld = ifoldMapOf (lined <.> folded) (uncurry classify)
  where
    classify y x = \case
        '|'  -> (M.singleton p TStraight, M.empty                    )
        '-'  -> (M.singleton p TStraight, M.empty                    )
        '/'  -> (M.singleton p TTurnNW  , M.empty                    )
        '\\' -> (M.singleton p TTurnNE  , M.empty                    )
        '+'  -> (M.singleton p TInter   , M.empty                    )
        'v'  -> (M.singleton p TStraight, M.singleton (SP p) (C DS 0))
        '^'  -> (M.singleton p TStraight, M.singleton (SP p) (C DN 0))
        '>'  -> (M.singleton p TStraight, M.singleton (SP p) (C DE 0))
        '<'  -> (M.singleton p TStraight, M.singleton (SP p) (C DW 0))
        _    -> mempty
      where
        p = V2 x y
