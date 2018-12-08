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
  , Scheduler(..)
  , Pop(..)
  , newScheduler
  , stepScheduler
  ) where

-- import           AOC2018.Solver       ((:~>)(..), dyno_)
-- import           Control.Lens
-- import           Control.Monad        (unless)
-- import           Control.Monad.State  (StateT, runStateT)
-- import           Control.Monad.Writer (Writer, execWriter, tell)
-- import           Data.Bifunctor       (first, second)
-- import           Data.Char            (ord, isUpper)
-- import           Data.Foldable        (fold, find, forM_, toList)
-- import           Data.Map             (Map)
-- import           Data.Semigroup       (Sum(..))
-- import           Data.Set             (Set)
-- import           Data.Set.NonEmpty    (NESet)
-- import           Data.Tuple           (swap)
-- import           Data.Witherable      (wither)
-- import           Numeric.Natural      (Natural)
import           AOC2018.Prelude
import           Control.Lens
import           Control.Monad.Writer
import           Data.These
import qualified Data.List.NonEmpty      as NE
import qualified Data.Map                as M
import qualified Data.Map.NonEmpty       as NEM
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


-- data BS2 a = BS2
--     { _bs2Deps    :: Map a (NESet a)
--     , _bs2Active  :: Map a Natural
--     , _bs2Waiting :: Set a
--     }

-- makeLenses ''BS2

-- buildSleigh
--     :: Ord a
--     => Int                -- ^ number of threads
--     -> Map a (Set a)      -- ^ children map
--     -> (a -> Natural)     -- ^ initializer
--     -> StateT (BS2 a) (Writer (Sum Int)) ()
-- buildSleigh cap childs starter = go
--   where
--     go = do
--       -- tick the clock
--       tell $ Sum 1

--       -- tick the threads, and get expired items
--       expired   <- bs2Active %%= tickAll

--       -- remove any expired dependencies from dependencies map
--       bs2Deps . wither        %= NES.nonEmptySet
--                                . (`S.difference` expired)
--                                . NES.toSet

--       -- add the dependencies of expired items to the queue
--       bs2Waiting              <>= foldMap (fold . (`M.lookup` childs)) expired

--       numToAdd <- uses bs2Active  $ (cap -) . M.size
--       deps     <- use  bs2Deps
--       eligible <- uses bs2Waiting $ S.filter (`M.notMember` deps)

--       -- take items from eligible waiting values to fill in the new gaps
--       let toAdd = S.take numToAdd eligible

--       -- add the items to the active threads
--       newActive <- bs2Active <<>= M.fromSet starter toAdd
--       -- delete the newly active items from the queue
--       bs2Waiting               %= (`S.difference` toAdd)

--       unless (M.null newActive) go

buildSleigh
    :: forall a. Ord a
    => Int
    -> (a -> Natural)
    -> Map a (Set a)
    -> Maybe Natural
buildSleigh cap waiter mp = NES.nonEmptySet (findRoots mp) <&> \rts ->
                              go (flipMap mp) S.empty (newScheduler cap waiter rts)
  where
    go  :: Map a (NESet a)
        -> Set a
        -> Scheduler a
        -> Natural
    go deps toAdd sched0 = case stepScheduler toAdd sched0 of
      Pop{..} ->
        let newDeps = flip mapMaybe deps $ NES.nonEmptySet
                                         . (`NES.difference` _popItems)
            newAdds = S.filter (`M.notMember` newDeps)
                    . foldMap (fold . (`M.lookup` mp))
                    $ _popItems
        in  _popTime + case _popSched of
              Just s  -> go newDeps newAdds s
              Nothing -> case NES.nonEmptySet newAdds of
                Nothing -> 0
                Just a  -> go newDeps S.empty $ newScheduler cap waiter a

day07b :: Map Char (Set Char) :~> Natural
day07b = MkSol
    { sParse = parseAll
    , sShow  = show
    , sSolve = \mp ->
        let waitTime          = fromIntegral
                              . (+ 1)
                              . (+ dyno_ "wait" 60)
                              . subtract (ord 'A')
                              . ord
        in  buildSleigh (dyno_ "cap" 5) waitTime mp
    }

data Scheduler a = MkSched
    { _schedQueue  :: !(Set a)
    , _schedActive :: !(NEMap a Natural)
    , _schedCap    :: !Int
    , _schedWaiter :: !(a -> Natural)
    }

newScheduler :: Int -> (a -> Natural) -> NESet a -> Scheduler a
newScheduler _schedCap _schedWaiter queue = MkSched{..}
  where
    (_schedQueue, _schedActive) = case NES.splitAt _schedCap queue of
      This addAll -> (S.empty, NEM.fromSet _schedWaiter addAll)
      That _      -> errorWithoutStackTrace "newScheduler: Capacity should be greater than 0"
      These toAdd toKeep -> (NES.toSet toKeep, NEM.fromSet _schedWaiter toAdd)

data Pop a = Pop { _popTime  :: !Natural
                 , _popItems :: !(NESet a)
                 , _popSched :: !(Maybe (Scheduler a))
                 }

stepScheduler
    :: Ord a
    => Set a
    -> Scheduler a
    -> Pop a
stepScheduler new sched = Pop{..}
  where
    newQueue        = _schedQueue sched <> new
    numToAdd        = _schedCap sched - NEM.size (_schedActive sched)
    (toAdd, toKeep) = S.splitAt numToAdd newQueue
    newActives      = NES.withNonEmpty
        (_schedActive sched)
        ((<> _schedActive sched) . NEM.fromSet (_schedWaiter sched))
        toAdd
    (toPop@((_,_popTime):|_) :| stillActive) = NE.groupWith1 snd
                                             . NE.sortWith snd
                                             . NEM.toList
                                             $ newActives
    _popItems      = NES.fromDistinctAscList . fmap fst $ toPop
    shiftedActives = M.map (subtract _popTime)
                   . M.fromDistinctAscList
                   . concatMap toList
                   $ stillActive
    _popSched      = NEM.nonEmptyMap shiftedActives <&> \a ->
                       sched { _schedQueue  = toKeep
                             , _schedActive = a
                             }

