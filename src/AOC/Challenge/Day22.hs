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

import           AOC.Common       (Point, mannDist, clearOut)
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
         | p == V2 0 0 -> 0
         | y == 0      -> x * 16807
         | x == 0      -> y * 48271
         | otherwise   -> fromIntegral (eLevs M.! V2 (x - 1) y)
                        * fromIntegral (eLevs M.! V2 x (y - 1))
    eLevs = modulo . fromIntegral . (+ d) <$> geoIxes

terrainTypes :: Int -> Point -> Point -> Map Point Terrain
terrainTypes d l = fmap (toEnum . (`mod` 3) . fromIntegral) . erosionLevels d l

parse22 :: String -> Maybe (Int, Point)
parse22 = go . map read . words . clearOut (not . isDigit)
  where
    go [d,x,y] = Just (d, V2 x y)
    go _       = Nothing

day22a :: (Int, Point) :~> Int
day22a = MkSol
    { sParse = parse22
    , sShow  = show
    , sSolve = \(d, p) -> Just . sum . fmap fromEnum $ terrainTypes d p p
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

neighbs :: Point -> [Point]
neighbs p = (p +) <$> [ V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0 ]

moves :: Map Point Terrain -> ClimbState -> [ClimbState]
moves mp (e0, p0) = filter (uncurry compat) $ es ++ ps
  where
    es = map (,p0)
       . filter (/= e0)
       $ [Nothing, Just EGear, Just ETorch]
    ps = (e0,) <$> neighbs p0
    compat e = maybe False (`compatible` e) . (`M.lookup` mp)

journey
    :: Map Point Terrain
    -> Point
    -> Maybe [ClimbState]
journey mp targ = (o:) <$> aStar (HS.fromList . moves mp)
                                 climbDist1
                                 (climbDist t)
                                 (== t)
                                 o
  where
    o = (Just ETorch, V2 0 0)
    t = (Just ETorch, targ)

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

pathTime :: [ClimbState] -> Int
pathTime = sum . map (uncurry climbDist1) . (zip <*> tail)

day22b :: _ :~> _
day22b = MkSol
    { sParse = parse22
    , sShow  = show
    , sSolve = \(d, p) ->
        let mp      = terrainTypes d (p * 2) p
        in  pathTime <$> journey mp p
    }
