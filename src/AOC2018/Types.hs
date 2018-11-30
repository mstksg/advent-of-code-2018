
module AOC2018.Types (
    Challenge
  , ChallengeMap
  , ChallengeSpec(..)
  ) where

import           Data.Finite
import           Data.Map    (Map)

type Challenge = String -> String

type ChallengeMap = Map (Finite 25) (Map Char Challenge)

data ChallengeSpec = CS { _csDay  :: Finite 25
                        , _csPart :: Char
                        }
