
module AOC2018.Types (
    Challenge(..)
  , withSolver, withSolver'
  , ChallengeError(..)
  , runChallenge
  , ChallengeMap
  , ChallengeSpec(..)
  ) where

import           Control.DeepSeq
import           Data.Finite
import           Data.Map        (Map)
import           GHC.Generics

data Challenge where
    MkC :: { cParse :: String -> Maybe a
           , cShow  :: b      -> String
           , cSolve :: a      -> Maybe b
           }
        -> Challenge

withSolver' :: (String -> String) -> Challenge
withSolver' f = withSolver (Just . f)

withSolver :: (String -> Maybe String) -> Challenge
withSolver f = MkC { cParse = Just
                   , cShow  = id
                   , cSolve = f
                   }

data ChallengeError = CEParse
                    | CESolve
  deriving Generic

instance NFData ChallengeError

runChallenge :: Challenge -> String -> Either ChallengeError String
runChallenge MkC{..} s = do
    x <- maybe (Left CEParse) Right $ cParse s
    y <- maybe (Left CESolve) Right $ cSolve x
    pure $ cShow y

type ChallengeMap = Map (Finite 25) (Map Char Challenge)

data ChallengeSpec = CS { _csDay  :: Finite 25
                        , _csPart :: Char
                        }
