{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day23
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day23 where

-- module AOC.Challenge.Day23 (
--     -- day23a
--   -- , day23b
--   ) where

import           AOC.Prelude
import           Data.Ix
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import qualified Data.Set           as S
import qualified Data.Set.NonEmpty  as NES
import qualified Linear             as L

type Point3 = V3 Int
data Circle = C { _cCenter :: !Point3
                , _cRadius :: !Int
                }
  deriving (Show, Eq, Ord)

day23a :: _ :~> _
day23a = MkSol
    { sParse = Just . parse23
    , sShow  = show
    , sSolve = \ps -> Just
                    . (`go` map _cCenter ps)
                    . maximumBy (comparing _cRadius)
                    $ ps
    }
  where
    go c = length . filter (`inRangeOf` c)

inRangeOf
    :: Point3
    -> Circle
    -> Bool
p `inRangeOf` (C c r) = mannDist c p <= r

findMostOverlaps
    :: NESet Circle
    -> (Int, Point3)
findMostOverlaps cs0 = maxOverlapInRegion (NES.toList cs0) bb0
  where
    bb0 = boundingBox3 . foldMap1 (NE.fromList . circleBounds) $ cs0

maxOverlapInRegion
    :: NonEmpty Circle
    -> V2 Point3
    -> (Int, Point3)
maxOverlapInRegion cs0 bb@(V2 mns mxs)
    | bbSize bb < 1000          = minimum . map manualCheck $ range (mns, mxs)
    | completeOverlap cs0 bb    = (length cs0, mns)
    | otherwise                 = case NE.nonEmpty (mapMaybe go . filter (/= bb) $ octants bb) of
        Nothing -> manualCheck mns
        Just os -> minimum os
  where
    manualCheck p = (length . NE.filter (p `inRangeOf`) $ cs0, p)
    go bb1 = (`maxOverlapInRegion` bb1) <$> NE.nonEmpty (NE.filter circInRegion cs0)
      where
        bb1Cube = boundingCube bb1
        circInRegion c = any (`inRangeOf` c) bb1Cube
                      || any (inRegion bb1) (circleBounds c)

inRegion :: V2 Point3 -> Point3 -> Bool
inRegion (V2 mn mx) p = all (>= 0) (p - mn)
                     && all (>= 0) (mx - p)


bbSize :: V2 Point3 -> Int
bbSize (V2 mn mx) = product $ mx - mn

octants :: V2 Point3 -> [V2 Point3]
octants (V2 mns mxs) = zipWith V2 (boundingCube (V2 mns mid))
                                  (boundingCube (V2 mid mxs))
  where
    mid = (\x y -> (x + y) `div` 2) <$> mns <*> mxs

completeOverlap
    :: Foldable f
    => f Circle
    -> V2 Point3
    -> Bool
completeOverlap cs0 = all inAll . boundingCube
  where
    inAll p = all (p `inRangeOf`) cs0

boundingCube
    :: V2 Point3
    -> [Point3]
boundingCube = traverse (\(V2 mn mx) -> [mn,mx]) . L.transpose


day23b :: _ :~> _
day23b = MkSol
    { sParse = NES.nonEmptySet . S.fromList . parse23
    , sShow  = show
    , sSolve = Just . sum . abs . snd . traceShowId . findMostOverlaps
    -- fmap (mannDist 0 . go ps . traceShowId . boundingBox3)
    --                 . NE.nonEmpty
    --                 . concatMap circleBounds
    --                 $ ps
    }
  -- where
  --   go :: [Circle] -> V2 Point3 -> Point3
  --   go ps (V2 mn mx) = fst
  --                    . minimumBy (comparing (sum . abs . fst))
  --                    . filter ((== maxOv) . snd)
  --                    $ ovs
  --     where
  --       ct p = length . filter (p `inRangeOf`) $ ps
  --       ovs = (\p -> (p, ct p)) <$> range (mn, mx)
  --       maxOv = maximum . map snd $ ovs

circleBounds :: Circle -> [Point3]
circleBounds (C c r) =
    [ c + d
    | b <- [ V3 r 0 0
           , V3 0 r 0
           , V3 0 0 r
           ]
    , d <- [b, -b]
    ]

boundingBox3 :: Foldable1 f => f Point3 -> V2 Point3
boundingBox3 ps = V3 xMin yMin zMin `V2` V3 xMax yMax zMax
  where
    ((Min xMin, Min yMin, Min zMin), (Max xMax, Max yMax, Max zMax)) = flip foldMap1 ps $ \(V3 x y z) ->
        ((Min x, Min y, Min z), (Max x, Max y, Max z))


parse23 :: String -> [Circle]
parse23 = mapMaybe (go . map read . words . clearOut d) . lines
  where
    d '-' = False
    d c   = not (isDigit c)
    go [x,y,z,r] = Just $ C (V3 x y z) r
    go _         = Nothing
