
module AOC.Common.Search (
    aStar
  ) where

import           Data.Map      (Map)
import           Data.OrdPSQ   (OrdPSQ)
import qualified Data.Map      as M
import qualified Data.OrdPSQ   as Q
import qualified Data.Sequence as Seq

data AStarState n p = AS { _asClosed  :: !(Map n (Maybe n))         -- ^ map of item to "parent"
                         , _asOpen    :: !(OrdPSQ n p (p, Maybe n))    -- ^ map of item to "parent", and cost-so-far
                         }

-- | A Star Search algorithm.
aStar
    :: forall n p. (Ord n, Ord p, Num p)
    => (n -> p)         -- ^ heuristic
    -> (n -> Map n p)   -- ^ neighborhood
    -> n                -- ^ start
    -> n                -- ^ target
    -> Maybe [n]        -- ^ the shortest path, if it exists
aStar h ex x0 dest = reconstruct <$> go (addBack x0 0 Nothing (AS M.empty Q.empty))
  where
    reconstruct :: Map n (Maybe n) -> [n]
    reconstruct mp = reverse $ goreco dest
      where
        goreco n = n : maybe [] goreco (mp M.! n)
    go :: AStarState n p -> Maybe (Map n (Maybe n))
    go as0@AS{..} = Q.minView _asOpen >>= \(n,_,(g,up),queue') ->
      let closed' = M.insert n up _asClosed
      in  if n == dest
            then Just closed'
            else go . M.foldlWithKey' (processNeighbor n g) (as0 { _asOpen = queue', _asClosed = closed'  })
                    $ ex n
    addBack :: n -> p -> Maybe n -> AStarState n p -> AStarState n p
    addBack x g up as0 = as0 { _asOpen = Q.insert x (g + h x) (g, up) . _asOpen $ as0 }
    processNeighbor :: n -> p -> AStarState n p -> n -> p -> AStarState n p
    processNeighbor curr currCost as0@AS{..} neighb moveCost
      | neighb `Q.member` _asOpen || neighb `M.member` _asClosed = as0
      | otherwise = addBack neighb (currCost + moveCost) (Just curr) as0
