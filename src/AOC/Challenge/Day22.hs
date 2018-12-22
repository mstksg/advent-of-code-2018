{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day22
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 22.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day22 (
    day22a
  , day22b
  ) where

import           AOC.Prelude hiding (aStar)
import           Data.Finite
import           Data.Graph.AStar
import           Data.Hashable      (Hashable)
import           Data.Ix
import qualified Data.HashSet       as HS
import qualified Data.Map           as M
import qualified Data.Set           as S

data Terrain = TRocky
             | TWet
             | TNarrow
  deriving (Eq, Ord, Show, Enum)

erosionLevel
    :: Int
    -> Point
    -> Point
    -> Maybe (Finite 20183)
erosionLevel d targ = eLev
  where
    eLev  = memoPoint eLev'
    eLev' = fmap (modulo . fromIntegral . (+ d)) . geoIxes
    geoIxes p@(V2 x y)
      | p == targ   = Just 0
      | p == V2 0 0 = Just 0
      | y == 0      = Just $ x * 16807
      | x == 0      = Just $ y * 48271
      | x <= 0      = Nothing
      | y <= 0      = Nothing
      | otherwise   = (*) <$> (fromIntegral <$> eLev (V2 (x - 1) y))
                          <*> (fromIntegral <$> eLev (V2 x (y - 1)))

terrainType
    :: Int
    -> Point
    -> Point
    -> Maybe Terrain
terrainType d targ = fmap (toEnum . (`mod` 3) . fromIntegral) . erosionLevel d targ

parse22 :: String -> Maybe (Int, Point)
parse22 = go . map read . words . clearOut (not . isDigit)
  where
    go [d,x,y] = Just (d, V2 x y)
    go _       = Nothing

day22a :: (Int, Point) :~> Int
day22a = MkSol
    { sParse = parse22
    , sShow  = show
    , sSolve = \(d, p) -> let tt = terrainType d p
                          in  Just . sum . map (maybe 0 fromEnum . tt) $ range (V2 0 0, p)
    }

data Equip = EGear
           | ETorch
  deriving (Eq, Ord, Show, Enum, Generic)

instance Hashable Equip

type ClimbState = (Maybe Equip, Point)

compatible :: Terrain -> Maybe Equip -> Bool
compatible TRocky  = isJust
compatible TWet    = (/= Just ETorch)
compatible TNarrow = (/= Just EGear )

moves :: (Point -> Maybe Terrain) -> ClimbState -> [ClimbState]
moves tt (e0, p0) = filter (uncurry compat) $ es ++ ps
  where
    es = map (,p0)
       . filter (/= e0)
       $ [Nothing, Just EGear, Just ETorch]
    ps = (e0,) <$> cardinalNeighbs p0
    compat e = maybe False (`compatible` e) . tt

journey
    :: (Point -> Maybe Terrain)
    -> Point
    -> Maybe [ClimbState]
journey tt targ = (o:) <$> aStar (HS.fromList . moves tt)
                                 climbDist
                                 (climbDist t)
                                 (== t)
                                 o
  where
    o = (Just ETorch, V2 0 0)
    t = (Just ETorch, targ)

climbDist :: ClimbState -> ClimbState -> Int
climbDist (e0,p0) (e1,p1)
    | e0 == e1  = dist p0 p1
    | otherwise = dist p0 p1 + 7

climbDist1 :: ClimbState -> ClimbState -> Int
climbDist1 (e0,_) (e1,_)
    | e0 == e1  = 1
    | otherwise = 7

dist :: Point -> Point -> Int
dist x y = sum . abs $ x - y

pathTime :: [ClimbState] -> Int
pathTime = sum . map (uncurry climbDist) . (zip`ap`tail)

day22b :: _ :~> _
day22b = MkSol
    { sParse = parse22
    , sShow  = show
    , sSolve = \(d, p) ->
        let tt      = terrainType d p
        in  pathTime <$> journey tt p
    }
