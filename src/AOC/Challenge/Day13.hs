{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day13
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day13 where

-- module AOC.Challenge.Day13 (
--     day13a
--   , day13b
--   ) where

import           AOC.Prelude
import           Control.Lens
import qualified Data.Map     as M
import qualified Data.Set     as S

type Point = V2 Int

data Track = THoriz
           | TVert
           | TTurnNW
           | TTurnNE
           | TInter
  deriving (Eq, Show, Ord)

-- True: is intersection, False: is not
type World = Map Point Track
data Dir = DN | DE | DS |DW
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance NFData Dir

data Cart = C { _cPos   :: Point
              , _cDir   :: Dir
              , _cLast  :: Maybe Point
              , _cTurns :: Int
              }
  deriving (Eq, Show, Generic)

instance NFData Cart

instance Ord Cart where
    compare = mconcat [ comparing (view _y . _cPos)
                      , comparing (view _x . _cPos)
                      , comparing _cDir
                      , comparing _cLast
                      , comparing _cTurns
                      ]

parseWorld :: String -> (Set Cart, World)
parseWorld = foldMap (uncurry parseLine)
           . zip [0..]
           . lines
  where
    parseLine y = foldMap (uncurry classify) . zip [0..]
      where
        classify x c
          | c == '|'      = (S.empty, M.singleton (V2 x y) TVert)
          | c == '-'      = (S.empty, M.singleton (V2 x y) THoriz)
          | c == '/'      = (S.empty, M.singleton (V2 x y) TTurnNW)
          | c == '\\'      = (S.empty, M.singleton (V2 x y) TTurnNE)
          | c ==     '+'     = (S.empty, M.singleton (V2 x y) TInter )
          | c ==     'v'     = (S.singleton (C (V2 x y) DS Nothing 0), M.singleton (V2 x y) TVert)
          | c ==     '^'     = (S.singleton (C (V2 x y) DN Nothing 0), M.singleton (V2 x y) TVert)
          | c ==     '>'     = (S.singleton (C (V2 x y) DE Nothing 0), M.singleton (V2 x y) THoriz)
          | c ==     '<'     = (S.singleton (C (V2 x y) DW Nothing 0), M.singleton (V2 x y) THoriz)
          | otherwise = mempty

-- \

(!?!) :: (Ord k, Show k) => Map k a -> k -> a
m !?! x = M.findWithDefault (error $ "bad key: " ++ show x) x m

stepCart :: World -> Cart -> Cart
stepCart w c = case M.lookup cNext w of
    Nothing    -> error $ "ran off track " ++ show c ++ " " ++ show cNext
    Just TTurnNW ->
      c { _cPos = cNext, _cDir = case _cDir c of DN -> DE
                                                 DE -> DN
                                                 DS -> DW
                                                 DW -> DS
        , _cLast = Just (_cPos c)
        }
    Just TTurnNE ->
      c { _cPos = cNext, _cDir = case _cDir c of DN -> DW
                                                 DW -> DN
                                                 DS -> DE
                                                 DE -> DS
        , _cLast = Just (_cPos c)
        }
    Just TInter    -> c { _cPos = cNext, _cDir = turnWith (_cTurns c) (_cDir c)
                        , _cTurns = _cTurns c + 1
                        , _cLast = Just (_cPos c)
                        }
    Just _         -> c { _cPos = cNext, _cLast = Just (_cPos c)}
  where
    cNext = _cPos c + dirPoint (_cDir c)

dirPoint :: Dir -> Point
dirPoint DN = V2 0 (-1)
dirPoint DE = V2 1 0
dirPoint DS = V2 0 1
dirPoint DW = V2 (-1) 0

turnWith :: Int -> Dir -> Dir
turnWith i = case i `mod` 3 of
    0 -> turnLeft
    1 -> id
    2 -> turnRight
    _ -> undefined

turnLeft :: Dir -> Dir
turnLeft DN = DW
turnLeft DE = DN
turnLeft DS = DE
turnLeft DW = DS

turnRight :: Dir -> Dir
turnRight DN = DE
turnRight DE = DS
turnRight DS = DW
turnRight DW = DN

stepCarts :: World -> Set Cart -> Either Point (Set Cart)
stepCarts w s0 = fmap snd . execStateT go $ (s0, S.empty)
  where
    go = uses _1 S.minView >>= \case
      Nothing -> pure ()
      Just (c, leftovers) -> do
        _1 .= leftovers
        let c' = stepCart w c
        allPoints <- gets (uncurry (<>))
        case find (collides c') allPoints of
          Nothing -> do
            _2 %= S.insert c'
            go
          Just d -> throwError $ _cPos d
    collides = (==) `on` _cPos

findCollision :: Set Cart -> Maybe Point
findCollision = fmap fst . find ((> 1) . snd) . M.toList . freqs . map _cPos . toList

runWorld :: World -> Set Cart -> Maybe Point
runWorld w = go
  where
    go s = case stepCarts w s of
      Left e   -> Just e
      Right s' -> go s'

day13a :: (World, Set Cart) :~> Point
day13a = MkSol
    { sParse = Just . swap . parseWorld
    , sShow  = \(V2 x y) -> show x ++ "," ++ show y
    , sSolve = uncurry runWorld
    }

-- TODO: do not strip

stepCarts2 :: World -> Set Cart -> Either Point (Set Cart)
stepCarts2 w s0 = fmap snd . execStateT go $ (s0, S.empty)
  where
    go = do
      () <- gets rnf
      uses _1 S.minView >>= \case
        Nothing -> pure ()
        Just (c, leftovers) -> do
          _1 .= leftovers
          let c' = stepCart w c
          allPoints <- gets (uncurry (<>))
          case find (collides c') allPoints of
            Nothing -> do
              _2 %= S.insert c'
              go
            Just d -> do
              _2 %= S.delete d
              _1 %= S.delete d
              case S.minView (S.delete d allPoints) of
                Nothing -> error "no carts left?"
                Just (lastCar, whatever)
                  | S.null whatever -> throwError $ _cPos lastCar
                  | otherwise       -> go
    collides = (==) `on` _cPos

runWorld2 :: World -> Set Cart -> Maybe Point
runWorld2 w = go
  where
    go s = case stepCarts2 w s of
      Left e   -> Just e
      Right s' -> go s'

day13b :: (World, Set Cart) :~> Point
day13b = MkSol
    { sParse = Just . swap . parseWorld
    , sShow  = \(V2 x y) -> show x ++ "," ++ show y
    , sSolve = uncurry runWorld2
    }

