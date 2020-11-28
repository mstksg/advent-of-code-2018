-- |
-- Module      : AOC.Challenge.Day06
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import           AOC.Common         (freqs, clearOut, Point, boundingBox, mannDist)
import           AOC.Solver         ((:~>)(..), dyno_)
import           Control.Monad      (guard, (<=<))
import           Data.Char          (isDigit)
import           Data.Functor       ((<&>))
import           Data.Ix            (range)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Witherable    (mapMaybe, catMaybes)
import           Linear             (V2(..))
import           Text.Read          (readMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import qualified Data.Set           as S

type Box   = V2 Point

bbPoints :: Box -> [Point]
bbPoints (V2 mins maxs) = range (mins, maxs)

labelVoronoi
    :: NonEmpty Point     -- ^ set of sites
    -> Point              -- ^ point to label
    -> Maybe Point        -- ^ the label, if unique
labelVoronoi sites p = do
    (closestSite, _) :| [] <- Just
                            . NE.head
                            . NE.groupWith1 snd
                            . NE.sortWith snd
                            $ dists
    pure closestSite
  where
    dists                  = sites <&> \site -> (site, mannDist p site)

day06a :: NonEmpty Point :~> Int
day06a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
  where
    onEdge :: Box -> Point -> Bool
    onEdge (V2 xMin yMin `V2` V2 xMax yMax) (V2 x y) =
            x `elem` [xMin, xMax]
         || y `elem` [yMin, yMax]

day06b :: NonEmpty Point :~> Int
day06b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
  where
    totalDist p = sum . fmap (mannDist p)

parseLine :: String -> Maybe Point
parseLine = (packUp =<<)
          . traverse readMaybe
          . words
          . clearOut (not . isDigit)
  where
    packUp [x,y] = Just $ V2 x y
    packUp _     = Nothing
