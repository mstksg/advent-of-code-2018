{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day11
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day11 where

-- module AOC.Challenge.Day11 (
--     day11a
--   , day11b
--   ) where

import           AOC.Prelude
import           Control.Lens
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import qualified Data.Map.NonEmpty  as NEM
import qualified Data.Set           as S
import qualified Data.Set.NonEmpty  as NES
import qualified Data.Vector        as V
import qualified Linear             as L

type Point = V2 Int

powerLevel :: Int -> Point -> Int
powerLevel sid (V2 x y) = pl3 - 5
  where
    rid = x + 10
    pl0 = rid * y
    pl1 = pl0 + sid
    pl2 = pl1 * rid
    pl3 = (pl2 `div` 100) `mod` 10

findMaxThree :: Map Point Int -> Point
findMaxThree mp = maximumBy (comparing go)
                        (range (V2 1 1, V2 298 298))
  where
    go p = sum [ mp M.! (p + shift)
               | shift <- range (V2 0 0, V2 2 2)
               ]

mkMap :: Int -> Map Point Int
mkMap i = M.fromSet (powerLevel i) . S.fromList $ range (V2 1 1, V2 300 300)

day11a :: Int :~> Point
day11a = MkSol
    { sParse = readMaybe
    , sShow  = \(V2 x y) -> show x ++ "," ++ show y
    , sSolve = \i -> Just . findMaxThree
                $ M.fromSet (powerLevel i) (S.fromList (range (V2 1 1, V2 300 300)))
    }

day11b :: Int :~> (Point, Int)
day11b = MkSol
    { sParse = readMaybe
    , sShow  = \(V2 x y, s) -> show x ++ "," ++ show y ++ "," ++ show s
    , sSolve = \i -> fmap findMaxAny
                   . NEM.nonEmptyMap
                   . M.fromSet (powerLevel i)
                   . S.fromList
                   $ range (V2 1 1, V2 300 300)
    }

findMaxAny :: NEMap Point Int -> (Point, Int)
findMaxAny = (\(Max (Arg _ res)) -> res)
           . (foldMap1 . foldMap1) (\(x, w) -> (Max (Arg w x)))
           . runningSquares

runningSquares :: NEMap Point Int -> NEMap Point (NonEmpty ((Point, Int), Int))
runningSquares mp = rusqa
  where
    rusqa = NEM.mapWithKey go mp
    go p0 x = case NEM.lookup (p0 - V2 1 1) rusqa of
        Nothing  -> newPoint :| []
        Just (force->(!sqs)) -> (newPoint :|) $ toList sqs >>= \((!p,!n),!t) -> do
            let !adds = sum [ mp NEM.! (p0 - shift)
                           | shift <- range (V2 1 0, V2 n 0)
                                   ++ range (V2 0 1, V2 0 n)
                           ]
                !newTot = t + adds + x
            guard $ newTot > t - 25
            pure ((p, n+1), newTot)
      where
        (force->(!newPoint)) = ((p0, 1), x)

-- kadane :: Num a => NonEmpty a -> ((Int, Int), a)
-- kadane (x :| xs) = go x x xs 0
--   where
--     go maxEndingHere maxSoFar y i
--       | y > maxEndingHere + y =
--     -- = _
--     --   where
--     --     newMaxEndingHere = max y (maxEndingHere + y)
--     --     newMaxSoFar      = max newMaxEndingHere maxSoFar
-- -- kadane mp = snd . M.findMax $ kad
-- --   where
-- --     kad :: Map Int ((Int, Int), Int)
-- --     kad


-- data Kadane = K { _maxEndingHere :: (Int, Int)
--                 , _maxSoFar      :: ((Point, Int), Int)
--                 }
--   deriving Show

-- kadane :: Map Point Int -> (Point, Int)
-- -- kadane = fst . _maxSoFar . snd . M.findMax . kadane_
-- kadane = undefined

-- kadane_ :: Map Point Int -> Map Point Kadane
-- kadane_ mp = kads
--   where
--     kads :: Map Point Kadane
--     kads = M.mapWithKey go mp
--     go :: Point -> Int -> Kadane
--     go p0 x = K { _maxEndingHere = newMaxEnding
--                 , _maxSoFar      = maximumBy (comparing snd) . concat $
--                                  [ maybeToList (_maxSoFar <$> upLeft)
--                                  , maybeToList (_maxSoFar <$> up  )
--                                  , maybeToList (_maxSoFar <$> left)
--                                  , [first mkSquare newMaxEnding]
--                                  ]
--                 }
--       where
--         up           = M.lookup (p0 - V2 0 1) kads
--         left         = M.lookup (p0 - V2 1 0) kads
--         upLeft       = M.lookup (p0 - V2 1 1) kads
--         newMaxEnding = case _maxEndingHere <$> upLeft of
--           Nothing -> (1, x)
--           Just (n, oldTot) ->
--             let adds = sum [ mp M.! (p0 - shift)
--                            | shift <- range (V2 1 0, V2 n 0)
--                                    ++ range (V2 0 1, V2 0 n)
--                            ]
--                 newTot = oldTot + adds + x
--             in  if x > newTot
--                   then (1    , x     )
--                   else (n + 1, newTot)
--         mkSquare n = (p0 - V2 (n - 1) (n - 1), n)

-- data Kadane1 = K1 { _maxEndingHere1 :: (Int       , Int)
--                   , _maxSoFar1      :: ((Int, Int), Int)
--                   }

-- kadane1_ :: Map Int Int -> Map Int (Int, Int)
-- kadane1_ mp = M.mapWithKey go mp
--   where
--     go :: Int -> Int -> (Int, Int)
--     go i x = _

    -- M.fromSet go . S.fromList . range $ (V2 1 1, V2 300 300)
    -- go :: Point -> Kadane
    -- go p0 = K { _maxEndingHere = newMaxEnding
    --           , _maxSoFar      = maximum $
    --                                 newMaxEnding
    --                               : maybeToList up
    --                              ++ maybeToList left
    --                              ++ (_maxSoFar <$> maybeToList upLeft)
    --           -- maximumBy (comparing (snd . _maxSoFar)) $
    --           --                          K here here
    --           --                        : maybeToList up
    --           --                       ++ maybeToList left
    --           --                       ++ maybeToList upLeft
    --           --                       ++ maybeToList extension
    --           }
    -- -- maximumBy (comparing (snd . _maxSoFar)) $
    -- --     K here here
    -- --   : maybeToList up
    -- --  ++ maybeToList left
    -- --  ++ maybeToList upLeft
    -- --  ++ maybeToList extension
    --   where
    --     here         = mp M.! p0
    --     up           = _maxSoFar <$> M.lookup (p0 - V2 0 1) kads
    --     left         = _maxSoFar <$> M.lookup (p0 - V2 0 1) kads
    --     upLeft       = M.lookup (p0 - V2 1 1) kads
    --     newMaxEnding = case upLeft of
    --       Nothing -> ((p0, 1), here)
    --       Just k  ->
    --         let ((pp, n),oldTot) = _maxEndingHere k
    --             adds = sum [ mp M.! (p0 - shift)
    --                        | shift <- range (V2 1 0, V2 n 0)
    --                                ++ range (V2 0 1, V2 0 n)
    --                        ]
    --             newTot = oldTot + adds + here
    --         in  if newTot >= oldTot
    --                 then ((pp, n + 1), newTot)
    --                 else ((p0, 1), here)
                    -- ((pp, n + 1), oldTot + adds + here)
        -- newMaxEnding = maximumBy _ [maxEndingCandidate, _maxEndingHere k]
        --         -- (_          , _
                --      )

    --     extension  = do
    --       k <- upLeft
    --       let n    = snd . fst $ _maxEndingHere k
    --           adds = sum . catMaybes $ [ M.lookup (p0 - shift) mp
    --                                    | shift <- range (V2 1 0, V2 n 0)
    --                                            ++ range (V2 0 1, V2 0 n)
    --                                    ]
    --           newPoint = _maxEndingHere k & _2 +~ adds
    --           newMax = maximumBy (comparing snd) [newPoint, _maxSoFar k]
    --       pure $ K newPoint newMax

--             upLeft <&> \k ->
--             let n      = snd . fst $ _maxEndingHere k
--                 newTot = sum . catMaybes $ [ M.lookup (p0 - shift) mp
--                                            | shift <- range (V2 1 0, V2 n 0)
--                                                    ++ range (V2 0 1, V2 0 n)
--                                            ]
--             in  K { _maxEndingHere = _maxEndingHere k
--                   }
        -- upLeftPlus = upLeft <&> \k ->
    -- go p0@(V2 x y) = fst . maximumBy (comparing snd) $
    --                     [ ((p0, 1), here)
    --                     ]
    --   where
    --     here = mp M.! p0
    --     up   = M.lookup (p0 - V2 0 1) kads <&> \(pu, nu) ->
    --     left = M.lookup (p0 - V2 1 0) kads

      --   here = mp M.! p0
      --   up   = M.findWithDefault (V2 x (y - 1), 0) (V2 x (y - 1)) kads
      --   addBottom = sum [ mp M.! (p0 + shift)
      --                   | shift <- range (V2 (-(snd up + 1)) 0, V2 0 0)
      --                   ]
      --   left = M.findWithDefault (V2 (x - 1) y, 0) (V2 (x - 1) y) kads
      --   addLeft = sum [ mp M.! (p0 + shift)
      --                 | shift <- range (V2 0 (-(snd left + 1)), V2 0 0)
      --                 ]



-- findMaxAny :: Map Point Int -> (Point, Int)
-- findMaxAny mp = fst
--               . maximumBy (comparing snd)
--               . map (\x -> (x, go x))
--               $ ((,) <$> range (V2 1 1, V2 300 300) <*> [1..300])
--   where
--     go (p, s) = sum [ M.findWithDefault 0 (p + shift) mp
--                     | shift <- range (V2 0 0, V2 (s - 1) (s - 1))
--                     ]

