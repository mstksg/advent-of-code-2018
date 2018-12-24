-- |
-- Module      : AOC.Prelude
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Custom Prelude while developing challenges.  Ideally, once challenges
-- are completed, an import to this module would be replaced with explicit
-- ones for future readers.
--


module AOC.Prelude (
    module P
  ) where

import           AOC.Common                as P
import           AOC.Common.Search         as P
import           AOC.Solver                as P
import           Control.Applicative       as P
import           Control.DeepSeq           as P
import           Control.Lens              as P (over, view, set)
import           Control.Monad             as P
import           Control.Monad.Except      as P
import           Control.Monad.Primitive   as P
import           Control.Monad.State       as P
import           Data.Bifunctor            as P
import           Data.Char                 as P
import           Data.Containers.ListUtils as P
import           Data.Either               as P
import           Data.Finite               as P (Finite, packFinite, getFinite, finites)
import           Data.Foldable             as P
import           Data.Function             as P
import           Data.Functor              as P
import           Data.IntMap               as P (IntMap)
import           Data.IntSet               as P (IntSet)
import           Data.Ix                   as P
import           Data.Kind                 as P
import           Data.List                 as P
import           Data.List.NonEmpty        as P (NonEmpty(..), nonEmpty)
import           Data.List.PointedList     as P (PointedList)
import           Data.Map                  as P (Map)
import           Data.Map.NonEmpty         as P (NEMap)
import           Data.Maybe                as P hiding (mapMaybe, catMaybes)
import           Data.Ord                  as P
import           Data.Profunctor           as P (Profunctor(..))
import           Data.Semigroup            as P
import           Data.Semigroup.Foldable   as P
import           Data.Set                  as P (Set)
import           Data.Set.NonEmpty         as P (NESet)
import           Data.Time                 as P
import           Data.Traversable          as P
import           Data.Tuple                as P
import           Data.Void                 as P
import           Data.Witherable           as P hiding (filter)
import           Debug.Trace               as P
import           GHC.Exts                  as P (sortWith, groupWith)
import           GHC.Generics              as P (Generic)
import           Linear                    as P (V0(..), V1(..), V2(..), V3(..), V4(..), R1(..), R2(..), R3(..), R4(..))
import           Numeric.Natural           as P
import           Text.Printf               as P
import           Text.Read                 as P (readMaybe)
