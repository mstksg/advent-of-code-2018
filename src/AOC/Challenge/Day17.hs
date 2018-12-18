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

module AOC.Challenge.Day17 where
-- module AOC.Challenge.Day17 (
--     day17a
--   , day17b
--   ) where

import           AOC.Prelude
import           Control.Lens
import           Data.Ix
import           Data.Functor.Alt
import qualified Data.Map               as M
import qualified Data.Set               as S

type Point = V2 Int

inBounds :: V2 Point -> Point -> Bool
inBounds (V2 xMin yMin `V2` V2 xMax yMax) (V2 x y) =
        x >= xMin && x <= xMax && y >= yMin && y <= yMax

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


fillWater :: Set Point -> Set Point
fillWater cl = S.filter (\p -> p ^. _y >= yMin && p ^. _y <= yMax)
             . M.keysSet
             $ drainMap cl yMax (V2 500 0)
  where
    V2 _ yMin `V2` V2 _ yMax  = boundingBox $ toList cl

day17a :: Set Point :~> _
day17a = MkSol
    { sParse = Just . foldMap parseVein . lines
    , sShow  = show
    , sSolve = Just . S.size . fillWater
    }

drainWater :: Set Point -> Set Point
drainWater cl = S.filter (\p -> p ^. _y >= yMin && p ^. _y <= yMax)
              . M.keysSet
              . M.filter not
              $ drainMap cl yMax (V2 500 0)
  where
    V2 _ yMin `V2` V2 _ yMax  = boundingBox $ toList cl


day17b :: Set Point :~> _
day17b = MkSol
    { sParse = Just . foldMap parseVein . lines
    , sShow  = show
    , sSolve = Just . S.size . drainWater
    }




parseVein :: String -> Set Point
parseVein ('x':(map read.words.clearOut(not.isDigit)->(x:y0:y1:_)))
    = S.fromList . map (V2 x) $ range (y0,y1)
parseVein ('y':(map read.words.clearOut(not.isDigit)->(y:x0:x1:_)))
    = S.fromList . map (`V2` y) $ range (x0,x1)
parseVein _ = S.empty

boundingBox :: [Point] -> V2 Point
boundingBox ps = V2 xMin yMin `V2` V2 xMax yMax
  where
    (Min xMin, Min yMin, Max xMax, Max yMax) = flip foldMap ps $ \(V2 x y) ->
        (Min x, Min y, Max x, Max y)

displayClay :: Set Point -> Set Point -> String
displayClay cl w = unlines
    [ [ maybe '.' label $ M.lookup (V2 x y) terrain
      | x <- [xMin .. xMax]
      ]
    | y <- [yMin .. yMax]
    ]
  where
    terrain = M.fromSet (const False) cl
           <> M.fromSet (const True) w
    label False = '#'
    label True  = '*'
    V2 xMin yMin `V2` V2 xMax yMax  = boundingBox . toList . M.keysSet $ terrain
