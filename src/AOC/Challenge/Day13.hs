{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

module AOC.Challenge.Day13 where

-- module AOC.Challenge.Day13 (
--     day13a
--   , day13b
--   ) where

import           AOC.Prelude
import           Control.Lens
import           Data.Functor.Foldable
import qualified Data.Map              as M
import qualified Data.Set              as S

type Point = V2 Int

data Track = TStraight
           | TTurnNW
           | TTurnNE
           | TInter
  deriving (Eq, Show, Ord)

type World = Map Point Track

data Dir = DN | DE | DS |DW
  deriving (Eq, Show, Ord, Enum, Bounded)

data Cart = C { _cPos   :: Point
              , _cDir   :: Dir
              , _cTurns :: Int
              }
  deriving (Eq, Show)

instance Ord Cart where
    compare = mconcat [ comparing (view _y . _cPos)
                      , comparing (view _x . _cPos)
                      , comparing _cDir
                      , comparing _cTurns
                      ]

makeLenses ''Cart

stepCart :: World -> Cart -> Cart
stepCart w c = case w M.! (c' ^. cPos) of
    TTurnNW   -> c' & cDir   %~ \case DN -> DE; DE -> DN; DS -> DW; DW -> DS
    TTurnNE   -> c' & cDir   %~ \case DN -> DW; DW -> DN; DS -> DE; DE -> DS
    TInter    -> c' & cDir   %~ turnWith (c ^. cTurns)
                    & cTurns +~ 1
    TStraight -> c'
  where
    c' = c & cPos +~ case c ^. cDir of
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

data CartLog a = CLCrash Point a
               | CLDone  Point
  deriving (Show, Functor)

stepCarts
    :: World
    -> Set Cart
    -> Fix CartLog
stepCarts w = ana (uncurry go) . (,S.empty)
  where
    go :: Set Cart -> Set Cart -> CartLog (Set Cart, Set Cart)
    go waiting done = case S.minView waiting of
      Nothing -> case S.minView done of
        Just (lastCar, S.null->True) -> CLDone (lastCar ^. cPos)
        _                            -> go done S.empty
      Just (stepCart w -> c, waiting') ->
        let allPoints = waiting' <> done
        in  case find (collides c) allPoints of
              Nothing -> go waiting' (S.insert c done)
              Just d  -> CLCrash (d ^. cPos) (S.delete d waiting', S.delete d done)
    collides = (==) `on` _cPos

firstCrash :: Fix CartLog -> Point
firstCrash = cata $ \case
    CLCrash p _ -> p
    CLDone  p   -> p

day13a :: (World, Set Cart) :~> Point
day13a = MkSol
    { sParse = Just . swap . parseWorld
    , sShow  = \(V2 x y) -> show x ++ "," ++ show y
    , sSolve = Just . firstCrash . uncurry stepCarts
    }

lastPoint :: Fix CartLog -> Point
lastPoint = cata $ \case
    CLCrash _ p -> p
    CLDone  p   -> p

day13b :: (World, Set Cart) :~> Point
day13b = MkSol
    { sParse = Just . swap . parseWorld
    , sShow  = \(V2 x y) -> show x ++ "," ++ show y
    , sSolve = Just . lastPoint . uncurry stepCarts
    }

parseWorld :: String -> (Set Cart, World)
parseWorld = foldMap (\(y, xs) -> foldMap (uncurry (classify y)) . zip [0..] $ xs)
           . zip [0..]
           . lines
  where
    classify y x = \case
      '|'  -> (S.empty                      , M.singleton (V2 x y) TStraight)
      '-'  -> (S.empty                      , M.singleton (V2 x y) TStraight)
      '/'  -> (S.empty                      , M.singleton (V2 x y) TTurnNW  )
      '\\' -> (S.empty                      , M.singleton (V2 x y) TTurnNE  )
      '+'  -> (S.empty                      , M.singleton (V2 x y) TInter   )
      'v'  -> (S.singleton (C (V2 x y) DS 0), M.singleton (V2 x y) TStraight)
      '^'  -> (S.singleton (C (V2 x y) DN 0), M.singleton (V2 x y) TStraight)
      '>'  -> (S.singleton (C (V2 x y) DE 0), M.singleton (V2 x y) TStraight)
      '<'  -> (S.singleton (C (V2 x y) DW 0), M.singleton (V2 x y) TStraight)
      _    -> mempty

