-- |
-- Module      : AOC2018.Types
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Types to drive the challenge runner and help speed up/clean up
-- solutions.
--

module AOC2018.Challenge (
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

-- | Abstracting over the type of a challenge solver to help with cleaner
-- solutions.
--
-- Consists of a parser, a shower, and a solver.  The solver solves
-- a general @a -> 'Maybe' b@ function, and the parser and shower are used
-- to handle the boilerplate of parsing and printing the solution.
data Challenge where
    MkC :: { cParse :: String -> Maybe a    -- ^ parse input into an @a@
           , cSolve :: a      -> Maybe b    -- ^ solve an @a@ input to a @b@ solution
           , cShow  :: b      -> String     -- ^ print out the @b@ solution in a pretty way
           }
        -> Challenge

-- | Construct a 'Challenge' from just a normal @String -> String@ solver.
-- Does no parsing or special printing treatment.
withSolver' :: (String -> String) -> Challenge
withSolver' f = withSolver (Just . f)

-- | Construct a 'Challenge' from a @String -> 'Maybe' String@
-- solver, which might fail.  Does no parsing or special printing
-- treatment.
withSolver :: (String -> Maybe String) -> Challenge
withSolver f = MkC { cParse = Just
                   , cShow  = id
                   , cSolve = f
                   }

-- | Errors that might happen when running a 'Challenge' on some input.
data ChallengeError = CEParse
                    | CESolve
  deriving (Show, Eq, Ord, Generic)

instance NFData ChallengeError

-- | Run a 'Challenge' on some input.
runChallenge :: Challenge -> String -> Either ChallengeError String
runChallenge MkC{..} s = do
    x <- maybe (Left CEParse) Right $ cParse s
    y <- maybe (Left CESolve) Right $ cSolve x
    pure $ cShow y

-- | A map of days to parts to challenges.
type ChallengeMap = Map (Finite 25) (Map Char Challenge)

-- | A specification for a specific challenge.  Should consist of a day and
-- a lowercase character.
data ChallengeSpec = CS { _csDay  :: Finite 25
                        , _csPart :: Char
                        }
