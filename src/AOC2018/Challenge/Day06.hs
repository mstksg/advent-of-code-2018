{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2018.Challenge.Day06
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC2018.Solver" for the types used in this module!

module AOC2018.Challenge.Day06 (
    day06a
  , day06b
  ) where

import           AOC2018.Prelude
import           Control.Lens
import           Linear          (V2(..))
import qualified Data.Ix         as Ix
import qualified Data.Map        as M
import qualified Data.Set        as S
import qualified Linear          as L

type Point = V2 Int
type Box   = V2 Point

parseLine :: String -> Maybe Point
parseLine = (packUp =<<) . traverse readMaybe . words . clearOut (not . isDigit)
  where
    packUp [x,y] = Just $ V2 x y
    packUp _     = Nothing

manhattan :: Point -> Point -> Int
manhattan x = sum . abs . subtract x

onEdge :: Box -> Point -> Bool
onEdge box pt = and $ elem <$> pt <*> L.transpose box

boundingBox :: Set Point -> Box
boundingBox ps = V2 bottomLeft topRight
  where
    xs         = S.mapMonotonic (view _1) ps
    ys         = S.map (view _2) ps
    minX       = S.findMin xs
    maxX       = S.findMax xs
    minY       = S.findMin ys
    maxY       = S.findMax ys
    bottomLeft = V2 minX minY
    topRight   = V2 maxX maxY

voronoi :: Box -> Set Point -> Map Point (Maybe Int)
voronoi (V2 mins maxs) ps = flip M.fromSet allPoints $ \p ->
    let dists                  = M.fromSet (manhattan p) ps
        Just (minVal, minDist) = minimumVal dists
        minIsRepeated          = M.size (M.filter (== minDist) dists) > 1
    in  S.findIndex minVal ps <$ guard minIsRepeated
  where
    allPoints = S.fromList $ Ix.range (mins, maxs)

day06a :: Set Point :~> Int
day06a = MkSol
    { sParse = fmap S.fromList . traverse parseLine . lines
    , sShow  = show
    , sSolve = \ps ->
        let bb    = boundingBox ps
            v     = M.mapMaybe id . voronoi bb $ ps
            edges = S.fromList . M.elems
                  . M.filterWithKey (\p _ -> onEdge bb p)
                  $ v
            areas = freqs $ M.filter (`S.notMember` edges) v
        in  Just . maximum . M.elems $ areas
    }

day06b :: [Point] :~> Int
day06b = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = \ps ->
        let V2 mins maxs = boundingBox $ S.fromList ps
            allPoints    = Ix.range (mins, maxs)
            predi p      = (< 10000) . sum . map (manhattan p) $ ps
        in  Just . length . filter predi $ allPoints
    }
