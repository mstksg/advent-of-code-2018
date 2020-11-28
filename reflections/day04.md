Day 4 was fun because it's something that, on the surface, sounds like it
requires a state machine to run through a stateful log and accumulate a bunch
of time sheets.

However, if we think of the log as just a stream of tokens, we can look at at
it as *parsing* this stream of tokens into time sheets -- no state or mutation
required.

First, the types at play:

```haskell
type Minute = Finite 60

type TimeCard = Map Minute Int

data Time = T { _tYear   :: Integer
              , _tMonth  :: Integer
              , _tDay    :: Integer
              , _tHour   :: Finite 24
              , _tMinute :: Minute
              }
  deriving (Eq, Ord)

newtype Guard = G { _gId :: Int }
  deriving (Eq, Ord)

data Action = AShift Guard
            | ASleep
            | AWake
```

Note that we have a bunch of "integer-like" quantities going on: the
year/month/day/hour/minute, the guard ID, and the "frequency" in the `TimeCard`
frequency map.  Just to help us accidentally not mix things up (like I
personally did many times), we'll make them all different types.  A `Minute` is
a `Finite 60` (`Finite 60`, from the *finite-typelits* library, is a type that
is basically the integers limited from 0 to 59).  Our hours are `Finite 24`.
Our Guard ID will be a newtype `Guard`, just so we don't accidentally mix it up
with other types.

Now, after parsing our input, we have a `Map Time Action`: a map of times to
actions committed at that time.  The fact that we store it in a `Map` ensures
that the log items are ordered and unique.

We now essentially want to parse a stream of `(Time, Action)` pairs into a `Map
Guard TimeCard`: A map of `TimeCard`s indexed by the guard that has that time
card.

To do that, we'll use the *parsec* library, which lets us parse over streams of
arbitrary token type.  Our parser type will take a `(Time, Action)` stream:

```haskell
import qualified Text.Parsec as P

type Parser = P.Parsec [(Time, Action)] ()
```

A `Parser Blah` will be a parser that, given a stream of `(Time, Action)`
pairs, will aggregate them into a value of type `Blah`.

Turning our stream into a `Map Guard TimeCard` is now your standard
run-of-the-mill parser combinator program.

```haskell
-- | We define a nap as an `ASleep` action followed by an `AWake` action.  The
-- result is a list of minutes slept.
nap :: Parser [Minute]
nap = do
    (T _ _ _ _ m0, ASleep) <- P.anyToken
    (T _ _ _ _ m1, AWake ) <- P.anyToken
    pure [m0 .. m1 - 1]     -- we can do this because m0 < m1 always in the
                            --   input data.

-- | We define a a guard's shift as a `AShift g` action, followed by
-- "many" naps.  The result is a list of minutes slept along with the ID of the
-- guard that slept them.
guardShift :: Parser (Guard, [Minute])
guardShift = do
    (_, AShift g) <- P.anyToken
    napMinutes    <- concat <$> many (P.try nap)
    pure (g, napMinutes)

-- | A log stream is many guard shifts. The result is the accumulation of all
-- of those shifts into a massive `Map Guard [Minute]` map, but turning all of
-- those [Minutes] into a frequency map instead by using `fmap freqs`.
buildTimeCards :: Parser (Map Guard TimeCard)
buildTimeCards = do
    shifts <- M.fromListWith (++) <$> many guardShift
    pure (fmap freqs shifts)
```

We re-use the handy `freqs :: Ord a => [a] -> Map a Int` function, to build a
frequency map, from Day 2.

We can run a parser on our `[(Time, Action)]` stream by using `P.parse ::
Parser a -> [(Time, Action)] -> SourceName -> Either ParseError a`.

The rest of the challenge involves "the X with the biggest Y" situations, which
all boil down to "The key-value pair with the biggest *some property of
value*".

We can abstract over this by writing a function that will find the key-value
pair with the biggest *some property of value*:

```haskell
import qualified Data.List.NonEmpty as NE

maximumValBy
    :: (a -> a -> Ordring)  -- ^ function to compare values
    -> Map k a
    -> Maybe (k, a)         -- ^ biggest key-value pair, using comparator function
maximumValBy c = fmap (maximumBy (c `on` snd)) . NE.nonEmpty . M.toList

-- | Get the key-value pair with highest value
maximumVal :: Ord a => Map k a -> Maybe (k, a)
maximumVal = maximumValBy compare
```

We use `fmap (maximumBy ...) . NE.nonEmpty` as basically a "safe maximum",
allowing us to return `Nothing` in the case that the map was empty. This works
because `NE.nonEmpty` will return `Nothing` if the list was empty, and `Just`
otherwise...meaning that `maximumBy` is safe since it is never given to a
non-empty list.

The rest of the challenge is just querying this `Map Guard TimeCard` using some
rather finicky applications of the predicates specified by the challenge.
Luckily we have our safe types to keep us from mixing up different concepts by
accident.

```haskell
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

day04a :: Map Time Action -> Maybe Int
day04a logs = do
    -- build time cards
    timeCards               <- eitherToMaybe $ P.parse buildTimeCards "" (M.toList logs)
    -- get the worst guard/time card pair, by finding the pair with the
    --   highest total minutes slept
    (worstGuard , timeCard) <- maximumValBy (comparing sum) timeCards
    -- get the minute in the time card with the highest frequency
    (worstMinute, _       ) <- maximumVal timeCard
    -- checksum
    pure $ _gId worstGuard * fromIntegral worstMinute

day04b :: Map Time Action -> Maybe Int
day04b logs = do
    -- build time cards
    timeCards                      <- eitherToMaybe $ P.parse buildTimeCards "" (M.toList logs)
    -- build a map of guards to their most slept minutes
    let worstMinutes :: Map Guard (Minute, Int)
        worstMinutes = M.mapMaybe maximumVal timeCards
    -- find the guard with the highest most-slept-minute
    (worstGuard, (worstMinute, _)) <- maximumValBy (comparing snd) worstMinutes
    -- checksum
    pure $ _gId worstGuard * fromIntegral worstMinute
```

Like I said, these are just some complicated queries, but they are a direct
translation of the problem prompt.  The real interesting part is the building
of the time cards, I think!  And not necessarily the querying part.

Parsing, again, can be done by stripping the lines of spaces and using
`words` and `readMaybe`s.  We can use `packFinite :: Integer -> Maybe (Finite
n)` to get our hours and minutes into the `Finite` type that `T` expects.

```haskell
parseLine :: String -> Maybe (Time, Action)
parseLine str = do
    [y,mo,d,h,mi] <- traverse readMaybe timeStamp
    t             <- T y mo d <$> packFinite h <*> packFinite mi
    a             <- case rest of
      "falls":"asleep":_ -> Just ASleep
      "wakes":"up":_     -> Just AWake
      "Guard":n:_        -> AShift . G <$> readMaybe n
      _                  -> Nothing
    pure (t, a)
  where
    (timeStamp, rest) = splitAt 5
                      . words
                      . clearOut (not . isAlphaNum)
                      $ str
```
