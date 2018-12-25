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

module AOC.Challenge.Day23 (
    day23a
  , day23b
  ) where

import           AOC.Prelude
import           Data.Ix
import           Data.OrdPSQ        (OrdPSQ)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import qualified Data.OrdPSQ        as PSQ
import qualified Data.Set           as S
import qualified Data.Set.NonEmpty  as NES
import qualified Linear             as L

type Point3 = V3 Int
type BoundingBox = V2 Point3
data Sphere = S { _sCenter :: !Point3
                , _sRadius :: !Int
                }
  deriving (Show, Eq, Ord)

day23a :: _ :~> _
day23a = MkSol
    { sParse = Just . parse23
    , sShow  = show
    , sSolve = \ps -> Just
                    . (`go` map _sCenter ps)
                    . maximumBy (comparing _sRadius)
                    $ ps
    }
  where
    go c = length . filter (`inRangeOf` c)

touchesRegion
    :: Sphere
    -> BoundingBox
    -> Bool
touchesRegion c bb = any (`inRangeOf` c) (boundingCube bb)
                  || any (`inRegion` bb) (circleBounds c)

inRangeOf
    :: Point3
    -> Sphere
    -> Bool
p `inRangeOf` S c r = mannDist c p <= r

inRegion :: Point3 -> BoundingBox -> Bool
inRegion p (V2 mn mx) = all (>= 0) (p - mn)
                     && all (>= 0) (mx - p)

splitOctants
    :: [Sphere]
    -> BoundingBox
    -> [(BoundingBox, [Sphere])]
splitOctants ss bb0 =
    [ (oct, touching)
    | oct <- octants bb0
    , let touching = filter (`touchesRegion` oct) ss
    ]

-- "drilling down" can only make number of drones smaller (or the same),
-- not larger.

drillDown
    :: NonEmpty Sphere
    -> Point3
drillDown ss0 = go (addIn bb0 ss0 PSQ.empty)
  where
    go :: OrdPSQ BoundingBox (Down Int) (NonEmpty Sphere) -> Point3
    go q0 = case PSQ.minView q0 of
      Nothing -> error "ran out of points? this shouldn't happen."
      Just (bb@(V2 mn mx), _, ss, q1)
        | mn == mx  -> mn
        | otherwise -> go $ foldl' (flip processNew) q1 (splitOctants (toList ss) bb)
    processNew (bb, ss) = case NE.nonEmpty ss of
      Nothing  -> id
      Just ss' -> addIn bb ss'
    addIn bb ss = PSQ.insert bb (Down (length ss)) ss
    bb0 = boundingBox3 . foldMap1 (NE.fromList . circleBounds) $ ss0

bbVolume :: BoundingBox -> Int
bbVolume (V2 mn mx) = product $ mx - mn

octants :: BoundingBox -> [BoundingBox]
octants (V2 mns mxs)
    | mns == mxs = []
    | otherwise  = filter (\(V2 mn mx) -> all (>= 0) (mx - mn))
                 $ zipWith V2 (boundingCube (V2 mns (mid + 1)))
                              (boundingCube (V2 mid mxs     ))
  where
    mid = (\x y -> (x + y) `div` 2) <$> mns <*> mxs

boundingCube
    :: BoundingBox
    -> [Point3]
boundingCube = traverse (\(V2 mn mx) -> [mn,mx]) . L.transpose

day23b :: _ :~> _
day23b = MkSol
    { sParse = NE.nonEmpty . parse23
    , sShow  = show
    , sSolve = Just . mannDist 0 . drillDown
    }

circleBounds :: Sphere -> [Point3]
circleBounds (S c r) =
    [ c + d
    | b <- [ V3 r 0 0
           , V3 0 r 0
           , V3 0 0 r
           ]
    , d <- [b, -b]
    ]

boundingBox3 :: Foldable1 f => f Point3 -> BoundingBox
boundingBox3 ps = V3 xMin yMin zMin `V2` V3 xMax yMax zMax
  where
    ((Min xMin, Min yMin, Min zMin), (Max xMax, Max yMax, Max zMax)) = flip foldMap1 ps $ \(V3 x y z) ->
        ((Min x, Min y, Min z), (Max x, Max y, Max z))


parse23 :: String -> [Sphere]
parse23 = mapMaybe (go . map read . words . clearOut d) . lines
  where
    d '-' = False
    d c   = not (isDigit c)
    go [x,y,z,r] = Just $ S (V3 x y z) r
    go _         = Nothing
