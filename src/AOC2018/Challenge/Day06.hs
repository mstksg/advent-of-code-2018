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

import           AOC2018.Solver          ((:~>)(..))
import           AOC2018.Util            (freqs, clearOut)
import           Control.Monad           (guard, (<=<))
import           Data.Char               (isDigit)
import           Data.Foldable           (minimumBy)
import           Data.Functor            ((<&>))
import           Data.Ix                 (range)
import           Data.List.NonEmpty      (NonEmpty(..))
import           Data.Ord                (comparing)
import           Data.Semigroup          (Min(..),Max(..))
import           Data.Semigroup.Foldable (foldMap1)
import           Data.Witherable         (mapMaybe, catMaybes)
import           Linear                  (V2(..))
import           Text.Read               (readMaybe)
import qualified Data.List.NonEmpty      as NE
import qualified Data.Map                as M
import qualified Data.Set                as S

type Point = V2 Int
type Box   = V2 Point

distance :: Point -> Point -> Int
distance x = sum . abs . subtract x

boundingBox :: NonEmpty Point -> Box
boundingBox ps = V2 xMin yMin `V2` V2 xMax yMax
  where
    (Min xMin, Min yMin, Max xMax, Max yMax) = flip foldMap1 ps $ \(V2 x y) ->
        (Min x, Min y, Max x, Max y)

bbPoints :: Box -> [Point]
bbPoints (V2 mins maxs) = range (mins, maxs)

labelVoronoi :: NonEmpty Point -> Point -> Maybe Point
labelVoronoi sites p = closestSite <$ guard (not minIsRepeated)
  where
    dists                  = sites <&> \site -> (site, distance p site)
    (closestSite, minDist) = minimumBy (comparing snd) dists
    minIsRepeated          = (> 1) . length . NE.filter ((== minDist) . snd) $ dists

day06a :: NonEmpty Point :~> Int
day06a = MkSol
    { sParse = (NE.nonEmpty <=< traverse parseLine) . lines
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

day06b :: NonEmpty Point :~> Int
day06b = MkSol
    { sParse = (NE.nonEmpty <=< traverse parseLine) . lines
    , sShow  = show
    , sSolve = \sites -> Just
                       . length
                       . filter ((< 10000) . (`totalDist` sites))
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
