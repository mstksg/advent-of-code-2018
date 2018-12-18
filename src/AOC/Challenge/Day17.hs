{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

neighbs :: Point -> Set Point
neighbs p = S.fromList . map (+ p) $ [V2 (-1) 0, V2 0 (-1), V2 1 0, V2 0 1]

inBounds :: V2 Point -> Point -> Bool
inBounds (V2 xMin yMin `V2` V2 xMax yMax) (V2 x y) =
        x >= xMin && x <= xMax && y >= yMin && y <= yMax

data Water = WFlow
           | WFlat
           | WRise
           | WSource
  deriving (Show, Eq, Ord)

data Terrain = TWater { _tWater :: Water }
             | TClay
  deriving (Show, Eq, Ord)

makeLenses ''Terrain

-- promote
--     :: Map Point Terrain
--     -> Point
--     -> Water
--     -> Water
-- promote mp p = \case
--     WFlow
--       | M.lookup down mp >= Just (TWater WRise)
--           && any ((>= Just (TWater WFlat)) . (`M.lookup` mp)) leftRight
--                                     -> WFlat
--       | otherwise                   -> WFlow
--     WFlat
--       | all ((>= Just (TWater WFlat)) . (`M.lookup` mp)) leftRight
--                                     -> WRise
--       | otherwise                   -> WFlat
--     WRise                           -> WRise
--     WSource                         -> WSource
--   where
--     down      = p + V2 0 1
--     leftRight = [p - V2 1 0, p + V2 1 0]

-- growWater
--     :: Point
--     -> Water
--     -> [Point]
-- growWater p = \case
--     WFlow   -> [p + V2 0 1]
--     WFlat   -> [p - V2 1 0, p + V2 1 0]
--     WRise   -> []
--     WSource -> [p + V2 0 1]

-- growWater
--     :: Set Point    -- ^ clay
--     -> Int          -- ^ max y
--     -> Set Point    -- ^ current water
--     -> Point        -- ^ place to start adding from
--     -> [Point]      -- ^ new item, if there is room
-- growWater cl ylim w0 = go
--   where
--     go !p
--       | p ^. _y > ylim  = Left SRCut
--       | otherwise       =
--             (p <$ guard' (p `S.notMember` w0))
--         <!> (go =<< addP p (V2 0 1)) `catchError` \case
--               SRCut       -> Left SRCut
--               SRBacktrack -> (growLeft  =<< addP p (V2 (-1) 0))
--                          <!> (growRight =<< addP p (V2 1 0))
--     growLeft !p =
--             (p <$ guard' (p `S.notMember` w0))
--         <!> (go =<< addP p (V2 0 1)) `catchError` \case
--               SRCut       -> Left SRCut
--               SRBacktrack -> (growLeft  =<< addP p (V2 (-1) 0))
--     growRight !p =
--             (p <$ guard' (p `S.notMember` w0))
--         <!> (go =<< addP p (V2 0 1)) `catchError` \case
--               SRCut       -> Left SRCut
--               SRBacktrack -> (growRight =<< addP p (V2 1 0))
--     addP :: Point -> Point -> Either StopReason Point
--     addP x y
--         | z `S.notMember` cl = Right z
--         | otherwise          = Left SRBacktrack
--       where
--         z = x + y
--     guard' :: Bool -> Either StopReason ()
--     guard' True  = Right ()
--     guard' False = Left SRBacktrack

-- data Growth a = GCut
--               | GBack
--               | GCons Point a
--

-- #..YYYYYYYYY...
-- #..Y#NNNNN#Y...
-- ...Y#NNNNN#Y...
-- ...Y#######Y...
-- ...Y.......Y...
-- YYYYYYYYYYYYYYY
-- Y#NNNNNNNNNNN#Y
-- Y#NNNNNNNNNNN#Y
-- Y#############Y

drainMap
    :: Set Point            -- ^ clay
    -> Int                  -- ^ max y
    -> Point                -- ^ starting point
    -> Map Point Bool       -- ^ map of water, and whether it is draining
drainMap cl ylim = flip execState M.empty . goDown
  where
    goDown p
      | p ^. _y > ylim = cache p $ pure True
      | otherwise      = cache p $
          goIfPossible p goDown (V2 0 1) >>= \case
            True  -> pure True
            False -> (||) <$> goIfPossible p goLeft  (V2 (-1) 0)
                          <*> goIfPossible p goRight (V2   1  0)
    goLeft  p = cache p $
      goIfPossible p goDown (V2 0 1) >>= \case
        True  -> pure True
        False -> goIfPossible p goLeft  (V2 (-1) 0)
    goRight p = cache p $
      goIfPossible p goDown (V2 0 1) >>= \case
        True  -> pure True
        False -> goIfPossible p goRight (V2   1  0)
    goIfPossible p f d
        | z `S.member` cl = cache p $ pure False
        | otherwise       = f z
      where
        z = p + d
    cache p act = gets (p `M.lookup`) >>= \case
      Just t  -> pure t
      Nothing -> do
        res <- act
        res <$ modify (M.insert p res)


fillWater :: Set Point -> Set Point
fillWater cl = S.filter (\p -> p ^. _y >= yMin && p ^. _y <= yMax)
             . M.keysSet
             $ drainMap cl yMax (V2 500 0)
  where
    V2 _ yMin `V2` V2 _ yMax  = boundingBox $ toList cl

-- #..TTTTTTTTT...
-- #..T#FFFFF#T...
-- ...T#FFFFF#T...
-- ...T#######T...
-- ...T.......T...
-- ...T.......T...
-- TTTTTTTTTTTTF#.
-- T#FFFFFFFFFFF#.
-- T#############.

day17a :: Set Point :~> _
day17a = MkSol
    { sParse = Just . foldMap parseVein . lines
    , sShow  = show
    , sSolve = Just . S.size . fillWater
    }

day17b :: _ :~> _
day17b = MkSol
    { sParse = Just
    , sShow  = id
    , sSolve = Just
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
