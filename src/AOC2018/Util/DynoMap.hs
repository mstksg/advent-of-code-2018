{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module AOC2018.Util.DynoMap (
    DynoMap(..)
  , lookupDyno
  , lookupDynoWith
  ) where

import           Control.Monad
import           Data.Dynamic
import           Data.Map      (Map)
import           Data.Maybe
import           Data.Proxy
import           GHC.TypeLits
import qualified Data.Map      as M

newtype DynoMap = Dyno { runDyno :: Map String Dynamic }
  deriving (Semigroup, Monoid)

-- | Lookup the value at a given key in a 'Dyno'.  Meant to be used with
-- type applications:
--
-- > lookupDyno @"hello"
lookupDyno
    :: forall (sym :: Symbol) a. (KnownSymbol sym, Typeable a)
    => DynoMap
    -> Maybe a
lookupDyno = fromDynamic
         <=< M.lookup (symbolVal (Proxy @sym))
           . runDyno

-- | Like 'lookupDyno', but with a default value to be returned if the key
-- is not found or has the wrong type.
lookupDynoWith
    :: forall (sym :: Symbol) a. (KnownSymbol sym, Typeable a)
    => a
    -> DynoMap
    -> a
lookupDynoWith def = fromMaybe def . lookupDyno @sym
