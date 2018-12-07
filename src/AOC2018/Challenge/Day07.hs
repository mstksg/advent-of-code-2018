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
  ) where

import           AOC2018.Solver       ((:~>)(..))
import           Control.Lens
import           Control.Monad        (unless)
import           Control.Monad.State  (StateT, runStateT)
import           Control.Monad.Writer (Writer, execWriter, tell)
import           Data.Bifunctor       (first, second)
import           Data.Char            (ord, isUpper)
import           Data.Foldable        (fold, find, forM_, toList)
import           Data.Map             (Map)
import           Data.Semigroup       (Sum(..))
import           Data.Set             (Set)
import           Data.Set.NonEmpty    (NESet)
import           Data.Tuple           (swap)
import           Data.Witherable      (wither)
import           Numeric.Natural      (Natural)
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Set.NonEmpty    as NES

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


data BS2 a = BS2
    { _bs2Deps    :: Map a (NESet a)
    , _bs2Active  :: Map a Natural
    , _bs2Waiting :: Set a
    }

makeLenses ''BS2

-- | Tick down all current threads. If any threads finish, take them out of
-- the map and put them into a set of finished results.
tickAll :: Map a Natural -> (Set a, Map a Natural)
tickAll = first M.keysSet . M.mapEither tick
  where
    tick i
        | i <= 0    = Left ()
        | otherwise = Right (i - 1)

buildSleigh
    :: Ord a
    => Map a (Set a)      -- ^ children map
    -> (a -> Natural)     -- ^ initializer
    -> StateT (BS2 a) (Writer (Sum Int)) ()
buildSleigh childs starter = go
  where
    go = do
      -- tick the clock
      tell $ Sum 1

      -- tick the threads, and get expired items
      expired   <- bs2Active %%= tickAll

      -- remove any expired dependencies from dependencies map
      bs2Deps . wither        %= NES.nonEmptySet
                               . (`S.difference` expired)
                               . NES.toSet

      -- add the dependencies of expired items to the queue
      bs2Waiting              <>= foldMap (fold . (`M.lookup` childs)) expired

      numToAdd <- uses bs2Active  $ (5 -) . M.size
      deps     <- use  bs2Deps
      eligible <- uses bs2Waiting $ S.filter (`M.notMember` deps)

      -- take items from eligible waiting values to fill in the new gaps
      let toAdd = S.take numToAdd eligible

      -- add the items to the active threads
      newActive <- bs2Active <<>= M.fromSet starter toAdd
      -- delete the newly active items from the queue
      bs2Waiting               %= (`S.difference` toAdd)

      unless (M.null newActive) go


day07b :: Map Char (Set Char) :~> Int
day07b = MkSol
    { sParse = parseAll
    , sShow  = show
    , sSolve = \mp -> Just $
        let (active, waiting) = S.splitAt 5 $ findRoots mp
        in  getSum . execWriter . runStateT (buildSleigh mp waitTime) $ BS2
                { _bs2Deps    = flipMap mp
                , _bs2Active  = M.fromSet waitTime active
                , _bs2Waiting = waiting
                }
    }
  where
    waitTime = fromIntegral . (+ 60) . subtract (ord 'A') . ord

