{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : AOC2018.Challenge.Day07
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC2018.Solver" for the types used in this module!

module AOC2018.Challenge.Day07 (
    day07a
  , day07b
  -- , Scheduler(..)
  -- , Pop(..)
  -- , newScheduler
  -- , stepScheduler
  ) where

import           AOC2018.Prelude
import           Control.Lens
import           Control.Monad.RWS
import           Control.Monad.Writer
import qualified Data.List.NonEmpty      as NE
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Set.NonEmpty       as NES


parseAll :: String -> Maybe (Map Char (Set Char))
parseAll = fmap (M.fromListWith (<>) . (map . second) S.singleton)
         . traverse parseLine
         . lines

parseLine :: String -> Maybe (Char, Char)
parseLine = pack . filter isUpper
  where
    pack [_,a,b] = Just (a, b)
    pack _       = Nothing


flipMap :: (Ord a, Ord b) => Map a (Set b) -> Map b (NESet a)
flipMap = M.fromListWith (<>)
        . map (second NES.singleton . swap)
        . concatMap (traverse toList)
        . M.toList

findRoots :: Ord a => Map a (Set a) -> Set a
findRoots mp = cs `S.difference` targs
  where
    cs = M.keysSet mp
    targs = S.unions $ toList mp


data BS1 a = BS1
    { _bs1Deps   :: Map a (NESet a)
    , _bs1Active :: Set a
    }

makeLenses ''BS1

lexicoTopo :: Ord a => Map a (Set a) -> StateT (BS1 a) (Writer [a]) ()
lexicoTopo childs = go
  where
    go = do
      deps   <- use bs1Deps
      active <- use bs1Active
      forM_ (find (`M.notMember` deps) active) $ \c -> do
        tell [c]
        bs1Deps . wither %= NES.nonEmptySet . NES.delete c
        bs1Active . at c .= Nothing
        bs1Active       <>= fold (M.lookup c childs)
        go

day07a :: Map Char (Set Char) :~> String
day07a = MkSol
    { sParse = parseAll
    , sShow  = id
    , sSolve = \mp -> Just . execWriter . runStateT (lexicoTopo mp) $ BS1
          { _bs1Deps   = flipMap mp
          , _bs1Active = findRoots mp
          }
    }



data Env a = Env { _envCap    :: Int
                 , _envWaiter :: a -> Natural
                 }

makeLenses ''Env

data Scheduler a = MkSched
    { _schedQueue  :: !(Set a)
    , _schedActive :: !(Map a Natural)
    }

makeClassy ''Scheduler

data BS2 a = BS2
    { _bs2Deps      :: Map a (NESet a)
    , _bs2Scheduler :: Scheduler a
    }

makeLenses ''BS2

instance HasScheduler (BS2 a) a where
    scheduler = bs2Scheduler

buildSleigh
    :: forall a m.
       ( Ord a
       , MonadState  (BS2 a) m
       , MonadReader (Env a) m
       , MonadWriter (Sum Natural) m
       )
    => Map a (Set a)
    -> m ()
buildSleigh childs = go (findRoots childs)
  where
    go toAdd = do
      popped <- stepScheduler toAdd
      forM_ (NES.nonEmptySet popped) $ \popped' -> do
        bs2Deps . wither %= NES.nonEmptySet . (`NES.difference` popped')
        deps   <- use bs2Deps

        go . S.filter (`M.notMember` deps)
           . foldMap (fold . (`M.lookup` childs))
           $ popped'

day07b :: Map Char (Set Char) :~> Natural
day07b = MkSol
    { sParse = parseAll
    , sShow  = show
    , sSolve = \mp ->
        let env = Env
              { _envCap    = dyno_ "cap" 5
              , _envWaiter = fromIntegral
                           . (+ 1)
                           . (+ dyno_ "wait" 60)
                           . subtract (ord 'A')
                           . ord
              }
        in  Just . getSum . view _3 . runRWS (buildSleigh mp) env $ BS2
              { _bs2Deps      = flipMap mp
              , _bs2Scheduler = emptyScheduler
              }
    }






-- | Scheduler Implementation


emptyScheduler :: Scheduler a
emptyScheduler = MkSched S.empty M.empty

stepScheduler
    :: ( Ord a
       , HasScheduler s a
       , MonadState  s m
       , MonadReader (Env a) m
       , MonadWriter (Sum Natural) m
       )
    => Set a
    -> m (Set a)    -- if empty, it means scheduler is exhausted
stepScheduler new = do
    cap      <- view envCap
    waiter   <- view envWaiter
    schedQueue <>= new
    numToAdd <- uses schedActive $ (cap -) . M.size
    toAdd    <- schedQueue %%= S.splitAt numToAdd
    active   <- schedActive <<>= M.fromSet waiter toAdd
    case NE.groupWith snd . sortOn snd $ M.toList active of
      [] -> pure S.empty
      toPop@((_,popTime):|_) : stillActive -> do
        schedActive .= ( M.map (subtract popTime)
                       . M.fromDistinctAscList
                       . concatMap toList
                       $ stillActive
                       )
        tell $ Sum popTime
        pure $ S.fromDistinctAscList . map fst . toList $ toPop

