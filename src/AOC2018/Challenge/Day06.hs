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

import           AOC2018.Solver    ((:~>)(..))
import           AOC2018.Util      (freqs, clearOut, minimumValNE)
import           Control.Lens      (view)
import           Control.Monad     (guard, (<=<))
import           Data.Char         (isDigit)
import           Data.Foldable     (toList)
import           Data.Ix           (range)
import           Data.Set.NonEmpty (NESet)
import           Data.Witherable   (mapMaybe, catMaybes)
import           Linear            (V2(..), _x, _y)
import           Text.Read         (readMaybe)
import qualified Data.Map          as M
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Set          as S
import qualified Data.Set.NonEmpty as NES

type Point = V2 Int
type Box   = V2 Point

distance :: Point -> Point -> Int
distance x = sum . abs . subtract x

boundingBox :: NESet Point -> Box
boundingBox ps = V2 xMin yMin `V2` V2 xMax yMax
  where
    xs         = NES.mapMonotonic (view _x) ps
    ys         = NES.map          (view _y) ps
    xMin       = NES.findMin xs
    xMax       = NES.findMax xs
    yMin       = NES.findMin ys
    yMax       = NES.findMax ys

bbPoints :: Box -> [Point]
bbPoints (V2 mins maxs) = range (mins, maxs)

labelVoronoi :: NESet Point -> Point -> Maybe Point
labelVoronoi sites p = closestSite <$ guard (not minIsRepeated)
  where
    dists                  = NEM.fromSet (distance p) sites
    (closestSite, minDist) = minimumValNE dists
    minIsRepeated          = (> 1) . length . filter (== minDist) . toList $ dists

day06a :: NESet Point :~> Int
day06a = MkSol
    { sParse = (NES.nonEmptySet . S.fromList <=< traverse parseLine) . lines
    , sShow  = show
    , sSolve = \sites -> Just $
        let bb    = boundingBox sites
            voron = catMaybes
                  . M.fromSet (labelVoronoi sites)
                  . S.fromList
                  . bbPoints
                  $ bb
            edges = S.fromList
                  . mapMaybe (\(point, site) -> site <$ guard (onEdge bb point))
                  . M.toList
                  $ voron
        in  maximum . freqs . M.filter (`S.notMember` edges) $ voron
    }
  where
    onEdge :: Box -> Point -> Bool
    onEdge (V2 xMin yMin `V2` V2 xMax yMax) (V2 x y) =
            x `elem` [xMin, xMax]
         || y `elem` [yMin, yMax]

day06b :: NESet Point :~> Int
day06b = MkSol
    { sParse = (NES.nonEmptySet . S.fromList <=< traverse parseLine) . lines
    , sShow  = show
    , sSolve = \sites -> Just
                       . length
                       . filter ((< 10000) . (`totalDist` NES.toList sites))
                       . bbPoints
                       . boundingBox
                       $ sites
    }
  where
    totalDist p = sum . fmap (distance p)


parseLine :: String -> Maybe Point
parseLine = (packUp =<<)
          . traverse readMaybe
          . words
          . clearOut (not . isDigit)
  where
    packUp [x,y] = Just $ V2 x y
    packUp _     = Nothing
