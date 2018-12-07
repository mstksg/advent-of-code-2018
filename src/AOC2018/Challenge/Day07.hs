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
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC2018.Prelude" imports to specific modules (with explicit
--     imports) for readability.

module AOC2018.Challenge.Day07 (
    day07a
  , day07b
  ) where

import           AOC2018.Prelude
import           Control.Lens
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Set.NonEmpty      as NES

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

findRoots :: Map Char (Set Char) -> Set Char
findRoots mp = cs `S.difference` targs
  where
    cs = M.keysSet mp
    targs = S.unions $ toList mp


data BS1 = BS1
    { _bs1Deps   :: Map Char (NESet Char)
    , _bs1Active :: Set Char
    }

makeLenses ''BS1

lexicoTopo :: Map Char (Set Char) -> State BS1 String
lexicoTopo childs = go
  where
    go = do
      deps   <- gets _bs1Deps
      active <- gets _bs1Active
      case find (`M.notMember` deps) active of
        Just c  -> do
          bs1Deps . wither %= NES.nonEmptySet . NES.delete c
          bs1Active . at c .= Nothing
          bs1Active       <>= fold (M.lookup c childs)
          (c:) <$> go
        Nothing -> pure []

day07a :: Map Char (Set Char) :~> String
day07a = MkSol
    { sParse = parseAll
    , sShow  = id
    , sSolve = \mp -> Just . evalState (lexicoTopo mp) $ BS1
          { _bs1Deps   = flipMap mp
          , _bs1Active = findRoots mp
          }
    }


waitTime :: Char -> Natural
waitTime = fromIntegral . (+ 60) . subtract (ord 'A') . ord

data BS2 = BS2
    { _bs2Deps    :: Map Char (NESet Char)
    , _bs2Active  :: Map Char Natural
    , _bs2Waiting :: Set Char
    }

makeLenses ''BS2

buildSleigh :: Map Char (Set Char) -> State BS2 Int
buildSleigh mp = go
  where
    go = do
      (M.keysSet -> expired)
                <- bs2Active %%= M.mapEither tick

      bs2Deps . wither        %= NES.nonEmptySet
                               . (`S.difference` expired)
                               . NES.toSet

      bs2Waiting              <>= foldMap (fold . (`M.lookup` mp)) expired

      numToAdd <- gets $ (5 -) . M.size . view bs2Active
      deps     <- gets $ view bs2Deps

      eligible <- bs2Waiting  %%= S.partition (`M.notMember` deps)

      let (toAdd, backToWaiting) = S.splitAt numToAdd eligible

      bs2Waiting              <>= backToWaiting
      newActive <- bs2Active <<>= M.fromSet waitTime toAdd

      if M.null newActive
         then pure 1
         else (+ 1) <$> go
    tick i
        | i <= 0    = Left ()
        | otherwise = Right (i - 1)


day07b :: Map Char (Set Char) :~> Int
day07b = MkSol
    { sParse = parseAll
    , sShow  = show
    , sSolve = \mp -> Just $
        let (active, waiting) = S.splitAt 5 $ findRoots mp
        in  evalState (buildSleigh mp) BS2
                { _bs2Deps    = flipMap mp
                , _bs2Active  = M.fromSet waitTime active
                , _bs2Waiting = waiting
                }
    }


