{-# OPTIONS_GHC -Wno-unused-imports   #-}

-- |
-- Module      : AOC.Challenge.Day17
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day17 (
    day17a
  , day17b
  ) where

import           AOC.Common              (clearOut, Point, boundingBox, displayAsciiMap)
import           AOC.Solver              ((:~>)(..))
import           Control.Lens            ((^.))
import           Control.Monad           (void, when)
import           Control.Monad.State     (gets, modify, execState)
import           Data.Char               (isDigit)
import           Data.Foldable           (toList)
import           Data.Ix                 (range)
import           Data.Map                (Map)
import           Data.Semigroup          (Min(..), Max(..))
import           Data.Semigroup.Foldable (toNonEmpty)
import           Data.Set                (Set)
import           Data.Set.NonEmpty       (NESet)
import           Linear                  (V2(..), _y)
import qualified Data.List.NonEmpty      as NE
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Set.NonEmpty       as NES

drainMap
    :: Set Point            -- ^ clay
    -> Int                  -- ^ max y
    -> Point                -- ^ starting point
    -> Map Point Bool       -- ^ map of water, and whether it is draining
drainMap cl ylim = flip execState M.empty . pourDown
  where
    pourDown p
      | p ^. _y > ylim = cache p $ pure True
      | otherwise      = cache p $
          goIfPossible p pourDown (V2 0 1) >>= \case
            True  -> pure True
            False -> do
              isDrain <- (||) <$> goIfPossible p (pourSide (-1)) (V2 (-1) 0)
                              <*> goIfPossible p (pourSide   1 ) (V2   1  0)
              when isDrain $ do
                void $ goIfPossible p (clearSide (-1)) (V2 (-1) 0)
                void $ goIfPossible p (clearSide   1 ) (V2   1  0)
              pure isDrain
    pourSide dx p = cache p $
      goIfPossible p pourDown (V2 0 1) >>= \case
        True  -> pure True
        False -> goIfPossible p (pourSide dx) (V2 dx 0)
    clearSide dx p = overrideCache p $ gets (p `M.lookup`) >>= \case
        Nothing    -> pure True
        Just True  -> pure True
        Just False -> True <$ goIfPossible p (clearSide dx) (V2 dx 0)
    goIfPossible p f d
        | z `S.member` cl = pure False
        | otherwise       = f z
      where
        z = p + d
    cache p act = gets (p `M.lookup`) >>= \case
      Just t  -> pure t
      Nothing -> overrideCache p act
    overrideCache p act = do
      res <- act
      res <$ modify (M.insert p res)


fillWater :: NESet Point -> Set Point
fillWater cl = S.filter (\p -> p ^. _y >= yMin && p ^. _y <= yMax)
             . M.keysSet
             $ drainMap (NES.toSet cl) yMax (V2 500 0)
  where
    V2 _ yMin `V2` V2 _ yMax  = boundingBox $ toNonEmpty cl

day17a :: NESet Point :~> Int
day17a = MkSol
    { sParse = NES.nonEmptySet . foldMap parseVein . lines
    , sShow  = show
    , sSolve = Just . S.size . fillWater
    }

drainWater :: NESet Point -> Set Point
drainWater cl = S.filter (\p -> p ^. _y >= yMin && p ^. _y <= yMax)
              . M.keysSet
              . M.filter not
              $ drainMap (NES.toSet cl) yMax (V2 500 0)
  where
    V2 _ yMin `V2` V2 _ yMax  = boundingBox $ toNonEmpty cl


day17b :: NESet Point :~> Int
day17b = MkSol
    { sParse = NES.nonEmptySet . foldMap parseVein . lines
    , sShow  = show
    , sSolve = Just . S.size . drainWater
    }




parseVein :: String -> Set Point
parseVein ('x':(map read.words.clearOut(not.isDigit)->(x:y0:y1:_)))
    = S.fromList . map (V2 x) $ range (y0,y1)
parseVein ('y':(map read.words.clearOut(not.isDigit)->(y:x0:x1:_)))
    = S.fromList . map (`V2` y) $ range (x0,x1)
parseVein _ = S.empty

_displayClay :: Set Point -> Set Point -> String
_displayClay cl w = displayAsciiMap '.' terrain
  where
    terrain = M.fromSet (const '#') cl
           <> M.fromSet (const '*') w
