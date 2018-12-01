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
  , SomeChallenge(..)
  , ChallengeError(..)
  , runChallenge
  , runSomeChallenge
  , ChallengeMap
  , ChallengeSpec(..)
  ) where

import           AOC2018.Util
import           Control.DeepSeq
import           Data.Finite
import           Data.Map        (Map)
import           GHC.Generics

-- | A specification for a specific challenge.  Should consist of a day and
-- a lowercase character.
data ChallengeSpec = CS { _csDay  :: Finite 25
                        , _csPart :: Char
                        }

-- | Abstracting over the type of a challenge solver to help with cleaner
-- solutions.
--
-- Consists of a parser, a shower, and a solver.  The solver solves
-- a general @a -> 'Maybe' b@ function, and the parser and shower are used
-- to handle the boilerplate of parsing and printing the solution.
data Challenge a b =
    MkC { cParse :: String -> Maybe a    -- ^ parse input into an @a@
        , cSolve :: a      -> Maybe b    -- ^ solve an @a@ input to a @b@ solution
        , cShow  :: b      -> String     -- ^ print out the @b@ solution in a pretty way
        }

-- | Construct a 'Challenge' from just a normal @String -> String@ solver.
-- Does no parsing or special printing treatment.
withSolver' :: (String -> String) -> Challenge String String
withSolver' f = withSolver (Just . f)

-- | Construct a 'Challenge' from a @String -> 'Maybe' String@
-- solver, which might fail.  Does no parsing or special printing
-- treatment.
withSolver :: (String -> Maybe String) -> Challenge String String
withSolver f = MkC { cParse = Just
                   , cShow  = id
                   , cSolve = f
                   }

data SomeChallenge where
    MkSC :: Challenge a b -> SomeChallenge

-- | Errors that might happen when running a 'Challenge' on some input.
data ChallengeError = CEParse
                    | CESolve
  deriving (Show, Eq, Ord, Generic)

instance NFData ChallengeError

-- | Run a 'Challenge' on some input.
runChallenge :: Challenge a b -> String -> Either ChallengeError String
runChallenge MkC{..} s = do
    x <- maybeToEither CEParse . cParse $ s
    y <- maybeToEither CESolve . cSolve $ x
    pure $ cShow y

-- | Run a 'Challenge' on some input.
runSomeChallenge :: SomeChallenge -> String -> Either ChallengeError String
runSomeChallenge (MkSC c) = runChallenge c

-- | A map of days to parts to challenges.
type ChallengeMap = Map (Finite 25) (Map Char SomeChallenge)
