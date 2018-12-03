-- |
-- Module      : AOC2018.Solver
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

module AOC2018.Solver (
    (:~>)(..)
  , withSolver, withSolver'
  , SomeSolution(..)
  , SolutionError(..)
  , runSolution
  , runSomeSolution
  , ChallengeMap
  , ChallengeSpec(..)
  , lookupSolution
  ) where

import           AOC2018.Util
import           Control.DeepSeq
import           Control.Monad
import           Data.Finite
import           Data.Map        (Map)
import           GHC.Generics
import qualified Data.Map        as M

-- | A specification for a specific challenge.  Should consist of a day and
-- a lowercase character.
data ChallengeSpec = CS { _csDay  :: Finite 25
                        , _csPart :: Char
                        }

-- | Abstracting over the type of a challenge solver to help with cleaner
-- solutions.
--
-- A @a ':~>' b@ encapsulates something that solves a challenge with input
-- type @a@ into a response of type @b@.
--
-- Consists of a parser, a shower, and a solver.  The solver solves
-- a general @a -> 'Maybe' b@ function, and the parser and shower are used
-- to handle the boilerplate of parsing and printing the solution.
data a :~> b = MkSol
    { sParse :: String -> Maybe a    -- ^ parse input into an @a@
    , sSolve :: a      -> Maybe b    -- ^ solve an @a@ input to a @b@ solution
    , sShow  :: b      -> String     -- ^ print out the @b@ solution in a pretty way
    }

-- | Wrap an @a ':~>' b@ and hide the type variables so we can put
-- different solutions in a container.
data SomeSolution where
    MkSomeSol :: a :~> b -> SomeSolution

-- | A map of days to parts to solutions.
type ChallengeMap = Map (Finite 25) (Map Char SomeSolution)

-- | Errors that might happen when running a ':~>' on some input.
data SolutionError = SEParse
                   | SESolve
  deriving (Show, Eq, Ord, Generic)

instance NFData SolutionError

-- | Construct a ':~>' from just a normal @String -> String@ solver.
-- Does no parsing or special printing treatment.
withSolver' :: (String -> String) -> String :~> String
withSolver' f = withSolver (Just . f)

-- | Construct a ':~>' from a @String -> 'Maybe' String@ solver, which
-- might fail.  Does no parsing or special printing treatment.
withSolver :: (String -> Maybe String) -> String :~> String
withSolver f = MkSol
    { sParse = Just
    , sShow  = id
    , sSolve = f
    }

-- | Run a ':~>' on some input.
runSolution :: a :~> b -> String -> Either SolutionError String
runSolution MkSol{..} s = do
    x <- maybeToEither SEParse . sParse $ s
    y <- maybeToEither SESolve . sSolve $ x
    pure $ sShow y

-- | Run a 'SomeSolution' on some input.
runSomeSolution :: SomeSolution -> String -> Either SolutionError String
runSomeSolution (MkSomeSol c) = runSolution c

-- | Lookup up a solution from a 'ChallengeMap'
lookupSolution :: ChallengeSpec -> ChallengeMap -> Maybe SomeSolution
lookupSolution CS{..} = M.lookup _csPart <=< M.lookup _csDay
