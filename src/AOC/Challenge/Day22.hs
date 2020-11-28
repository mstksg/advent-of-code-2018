-- |
-- Module      : AOC.Challenge.Day22
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day22 (
    day22a
  , day22b
  ) where

import           AOC.Common       (Point, mannDist, clearOut, cardinalNeighbs)
import           AOC.Solver       ((:~>)(..))
import           Data.Char        (isDigit)
import           Data.Finite      (Finite, modulo)
import           Data.Graph.AStar (aStar)
import           Data.Hashable    (Hashable)
import           Data.Ix          (range)
import           Data.Map         (Map)
import           Data.Maybe       (isJust)
import           GHC.Generics     (Generic)
import           Linear           (V2(..))
import qualified Data.HashSet     as HS
import qualified Data.Map         as M
import qualified Data.Set         as S

data Terrain = TRocky
             | TWet
             | TNarrow
  deriving (Eq, Ord, Show, Enum)


erosionLevels :: Int -> Point -> Point -> Map Point (Finite 20183)
erosionLevels d lim targ = eLevs
  where
    geoIxes = (`M.fromSet` S.fromList (range (V2 0 0, lim))) $ \p@(V2 x y) ->
      if | p == targ   -> 0
         | y == 0      -> x * 16807
         | x == 0      -> y * 48271
         | otherwise   -> fromIntegral (eLevs M.! V2 (x - 1) y)
                        * fromIntegral (eLevs M.! V2 x (y - 1))
    eLevs = modulo . fromIntegral . (+ d) <$> geoIxes

terrainTypes :: Int -> Point -> Point -> Map Point Terrain
terrainTypes d l = fmap (toEnum . (`mod` 3) . fromIntegral) . erosionLevels d l

day22a :: (Int, Point) :~> Int
day22a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

data Equipment = EGear
               | ETorch
  deriving (Eq, Ord, Show, Enum, Generic)
instance Hashable Equipment

type ClimbState = (Maybe Equipment, Point)

compatible :: Terrain -> Maybe Equipment -> Bool
compatible TRocky  = isJust
compatible TWet    = (/= Just ETorch)
compatible TNarrow = (/= Just EGear )

moves :: Map Point Terrain -> ClimbState -> [ClimbState]
moves mp (e0, p0) = filter (uncurry compat) $ es ++ ps
  where
    es = [ (e1, p0)
         | e1 <- [Nothing, Just EGear, Just ETorch], e1 /= e0
         ]
    ps = (e0,) <$> cardinalNeighbs p0
    compat e = maybe False (`compatible` e) . (`M.lookup` mp)

journey
    :: Map Point Terrain
    -> Point
    -> Maybe [ClimbState]
journey mp targ = (o:) <$>
    aStar (HS.fromList . moves mp) climbDist1 (climbDist t) (== t) o
  where
    o = (Just ETorch, V2 0 0)
    t = (Just ETorch, targ)

-- | Used as the A* heuristic: best-case-scenario time for arriving at
-- destination with correct tool.
climbDist :: ClimbState -> ClimbState -> Int
climbDist (e0,p0) (e1,p1)
    | e0 == e1  = mannDist p0 p1
    | otherwise = mannDist p0 p1 + 7

-- | A version of 'climbDist' that works for a single move or equipment
-- swap only.
climbDist1 :: ClimbState -> ClimbState -> Int
climbDist1 (e0,_) (e1,_)
    | e0 == e1  = 1
    | otherwise = 7

day22b :: (Int, Point) :~> Int
day22b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
  where
    pad (V2 x y)
      | x > y     = V2 ((x * 5) `div` 4) (y * 2)
      | otherwise = V2 (x * 2)           ((y * 5) `div` 4)
    pathTime = sum . map (uncurry climbDist1) . (zip <*> tail)

parse22 :: String -> Maybe (Int, Point)
parse22 = go . map read . words . clearOut (not . isDigit)
  where
    go [d,x,y] = Just (d, V2 x y)
    go _       = Nothing

