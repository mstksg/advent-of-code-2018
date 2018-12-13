Reflections
===========

[Table of Contents][]

[Table of Contents]: https://github.com/mstksg/advent-of-code-2018#reflections-and-benchmarks

Day 1
-----

*[Prompt][d01p]* / *[Code][d01g]* / *[Rendered][d01h]*

[d01p]: https://adventofcode.com/2018/day/1
[d01g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day01.hs
[d01h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day01.html

Day 1 is a pretty straightforward functional programming sort of pipeline.

The first part is just a sum:

```haskell
day01a :: [Int] -> Int
day01a = sum
```

The second part is a little tricker, but we can get a list of running sums with
`scanl (+) 0`.  We need to find the first *repeated* item in that list of
running totals.  We can do this using explicit recursion down the linked list:

```haskell
import qualified Data.Set as S

firstRepeated :: [Int] -> Maybe Int
firstRepeated = go S.empty
  where
    go seen (x:xs)
      | x `S.member` seen = Just x                      -- this is it, chief
      | otherwise         = go (x `S.insert` seen) xs   -- we have to look furhter
```

And so then we have our full pipeline.  We do need to remember to loop the input
list infinitely by using `cycle`.

```haskell
day01b :: [Int] -> Maybe Int
day01b = firstRepeated . scanl (+) 0 . cycle
```

We do need a parser, and we can leverage `readMaybe`:

```haskell
parseItem :: String -> Maybe Int
parseItem = readMaybe . filter (/= '+')

parseList :: String -> Maybe [Int]
parseList = traverse parseItem . lines
```

One small extra bonus note --- as a Haskeller, we are always taught to be
afraid of explicit recursion.  So, the implementation of `firstRepeated` is a
little unsettling.  We can write it using a catamorphism instead, from the
*recursion-schemes* library:

```haskell
firstRepeated :: [Int] -> Maybe Int
firstRepeated xs = cata go xs S.empty
  where
    go  :: ListF Int (Set Int -> Maybe Int)
        -> Set Int
        -> Maybe Int
    go Nil _              = Nothing
    go (Cons x searchRest) seen
      | x `S.member` seen = Just x                          -- this is it, chief
      | otherwise         = searchRest (x `S.insert` seen)  -- we have to look further
```

`cata` wraps up a very common sort of recursion, so we can safely write our
`firstRepeated` as a non-recursive function.

### Day 1 Benchmarks

```
>> Day 01a
benchmarking...
time                 2.937 ms   (2.916 ms .. 2.962 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.941 ms   (2.923 ms .. 2.964 ms)
std dev              65.38 μs   (52.82 μs .. 82.12 μs)

>> Day 01b
benchmarking...
time                 143.4 ms   (138.3 ms .. 148.1 ms)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 149.6 ms   (147.0 ms .. 158.3 ms)
std dev              5.845 ms   (987.7 μs .. 8.857 ms)
variance introduced by outliers: 12% (moderately inflated)
```

Day 2
-----

*[Prompt][d02p]* / *[Code][d02g]* / *[Rendered][d02h]*

[d02p]: https://adventofcode.com/2018/day/2
[d02g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day02.hs
[d02h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day02.html

Day 2 part 1 works out nicely in a functional paradigm because it can be seen
as just building a couple of frequency tables.

I often use this function to generate a frequency table of values in a list:

```haskell
import qualified Data.Map as M

freqs :: [a] -> Map a Int
freqs = M.fromListWith (+) . map (,1)
```

Day 2 part 1 is then to:

1.  Build a frequency map for chars for each line
2.  Aggregate all of the seen frequencies in each line
3.  Build a frequency map of the seen frequencies
4.  Look up how often freq 2 and freq 3 occurred, and then multiply

So we have:

```haskell
day02a :: [String] -> Maybe Int
day02a = mulTwoThree
       . freqs
       . concatMap (nubOrd . M.elems . freqs)

mulTwoThree :: Map Int Int -> Maybe Int
mulTwoThree mp = (*) <$> M.lookup 2 mp <*> M.lookup 3 mp
```

Part 2 for this day is pretty much the same as Part 2 for day 1, only instead
of finding the first item that has already been seen, we find the first item
who has any *neighbors* who had already been seen.

```haskell
import           Control.Lens
import qualified Data.Set as S

firstNeighbor :: [String] -> Maybe (String, String)
firstNeighbor = go S.empty
  where
    go seen (x:xs) = case find (`S.member` seen) (neighbors x) of
        Just n  -> Just (x, n)
        Nothing -> go (x `S.insert` seen) xs
    go _ [] = Nothing

neighbors :: String -> [String]
neighbors xs = [ xs & ix i .~ newChar
               | i       <- [0 .. length xs - 1]
               | newChar <- ['a'..'z']
               ]
```

`firstNeighbor` will return the first item who has a neighbor that has already
been seen, along with that neighbor.

The answer we need to return is the common letters between the two strings, so
we can write a function to only keep common letters between two strings:

```haskell
onlySame :: String -> String -> String
onlySame xs = catMaybes . zipWith (\x y -> x <$ guard (x == y)) xs

-- > onlySame "abcd" "abed" == "abd"
```

And that's pretty much the entire pipeline:

```haskell
day02a :: [String] -> Maybe String
day02a = fmap (uncurry onlySame) . firstNeighbor
```

Parsing is just `lines :: String -> [String]`, which splits a string on lines.

### Day 2 Benchmarks

```
>> Day 02a
benchmarking...
time                 1.317 ms   (1.271 ms .. 1.392 ms)
                     0.982 R²   (0.966 R² .. 0.999 R²)
mean                 1.324 ms   (1.298 ms .. 1.373 ms)
std dev              115.5 μs   (77.34 μs .. 189.0 μs)
variance introduced by outliers: 65% (severely inflated)

>> Day 02b
benchmarking...
time                 69.61 ms   (68.29 ms .. 72.09 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 69.08 ms   (68.47 ms .. 69.99 ms)
std dev              1.327 ms   (840.8 μs .. 1.835 ms)
```

Day 3
-----

*[Prompt][d03p]* / *[Code][d03g]* / *[Rendered][d03h]*

[d03p]: https://adventofcode.com/2018/day/3
[d03g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day03.hs
[d03h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day03.html

Day 3 brings back one of my favorite data structures in Haskell -- `Map (Int,
Int)`!  It's basically a sparse grid.  It maps coordinates to values at each
coordinate.

We're going to use `V2 Int` (from *[linear][]*) instead of `(Int, Int)` (they're
the same thing), because we get to use the overloaded `+` operator to do
point-wise addition.  Let's also define a rectangle specification and claim
record type to keep things clean:

[linear]: https://hackage.haskell.org/package/linear

```haskell
type Coord = V2 Int

data Rect = R { rStart :: Coord
              , rSize  :: Coord
              }

data Claim = C { cId   :: Int
               , cRect :: Rect
               }
```

Now, we want to make a function that, given a rectangle, produces a list of
every coordinate in that rectangle.  We can take advantage of `range` from
*Data.Ix*, which enumerates all coordinates between two corners:

```haskell
tiles :: Rect -> [Coord]
tiles (R start size) = range (topLeft, bottomRight)
  where
    topLeft     = start
    bottomRight = start + size - 1          -- V2 has a Num instance
```

Now we can stake all of the claims and lay all of the tiles down into a `Map
Coord Int`, a frequency map of coordinates that have been claimed (and how many
times they have been claimed):

```haskell
layTiles :: [Rect] -> Map Coord Int
layTiles = freqs . concatMap tiles
```

(Reusing `freqs` from Day 2)

From there, we need to count how many frequencies we observe are greater
than 1.  We can do that by filtering and counting how many are left.

```haskell
import qualified Data.Map as M

day03a :: [Rect] -> Int
day03a = length . filter (>= 2) . M.elems . layTiles
```

For `day03`, we can use `find` to search our list of claims by id's,
`[(Int, Rect)]` and find any claim that is completely non-overlapping.

We can check if a claim is non-overlapping or not by checking our map of staked
tiles and making sure that every square in the claim has exactly frequency `1`.

```haskell
noOverlap :: Map Coord Int -> Rect -> Bool
noOverlap tilesClaimed r = all isAlone (tiles r)
  where
    isAlone c = M.lookup c tilesClaimed == Just 1
```

And that's our Part 2:

```haskell
day03b :: [Claim] -> Maybe Int
day03b ts = cId <$> find (noOverlap stakes . cRect) ts
  where
    stakes = layTiles (map snd ts)
```

Parsing for this one is a little tricky, but we can get away with just clearing
out all non-digit characters and using `words` to split up a string into its
constituent words, and `readMaybe` to read each one.

```haskell
parseLine :: String -> Maybe Claim
parseLine = mkLine
          . mapMaybe readMaybe
          . words
          . map onlyDigits
  where
    mkLine [i,x0,y0,w,h] = Just $ Claim i (R (V2 x0 y0) (V2 w h))
    mkLine _             = Nothing
    onlyDigits c
      | isDigit c = c
      | otherwise = ' '
```

### Day 3 Benchmarks

```
>> Day 03a
benchmarking...
time                 450.0 ms   (NaN s .. 504.8 ms)
                     0.994 R²   (0.982 R² .. 1.000 R²)
mean                 519.3 ms   (484.5 ms .. 586.5 ms)
std dev              66.50 ms   (855.7 μs .. 79.42 ms)
variance introduced by outliers: 24% (moderately inflated)

>> Day 03b
benchmarking...
time                 464.0 ms   (260.4 ms .. 586.1 ms)
                     0.974 R²   (NaN R² .. 1.000 R²)
mean                 493.5 ms   (460.4 ms .. 525.2 ms)
std dev              36.73 ms   (31.40 ms .. 38.23 ms)
variance introduced by outliers: 21% (moderately inflated)
```

Day 4
-----

*[Prompt][d04p]* / *[Code][d04g]* / *[Rendered][d04h]*

[d04p]: https://adventofcode.com/2018/day/4
[d04g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day04.hs
[d04h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day04.html

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

### Day 4 Benchmarks

```
>> Day 04a
benchmarking...
time                 21.54 ms   (19.91 ms .. 22.43 ms)
                     0.909 R²   (0.721 R² .. 1.000 R²)
mean                 22.43 ms   (21.30 ms .. 26.68 ms)
std dev              4.602 ms   (376.2 μs .. 9.075 ms)
variance introduced by outliers: 79% (severely inflated)

>> Day 04b
benchmarking...
time                 20.90 ms   (19.88 ms .. 23.04 ms)
                     0.924 R²   (0.813 R² .. 0.999 R²)
mean                 20.80 ms   (20.09 ms .. 22.76 ms)
std dev              2.790 ms   (664.4 μs .. 4.922 ms)
variance introduced by outliers: 60% (severely inflated)
```

Day 5
-----

*[Prompt][d05p]* / *[Code][d05g]* / *[Rendered][d05h]* / *[Blog][d05b]*

[d05p]: https://adventofcode.com/2018/day/5
[d05g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day05.hs
[d05h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day05.html
[d05b]: https://blog.jle.im/entry/alchemical-groups.html

**My write-up for this is actually [on my blog, here][d05b]!**  It involves my
group theory/free group/group homomorphism based solution.  That's my main
reflection, but I also had a method that I wrote *before*, that I would still
like to preserve.

So, preserved here was my original solution involving `funkcyCons` and `foldr`:

One of the first higher-order functions you learn about in Haskill is `foldr`,
which is like a "skeleton transformation" of a list.

That's because in Haskell, a (linked) list is one of two constructors: nil
(`[]`) or cons (`:`).  The list `[1,2,3]` is really `1:(2:(3:[]))`.

`foldr f z` is a function that takes a list replaces all `:`s with `f`, and
`[]`s with `z`s:

```haskell
          [1,2,3] = 1  :  (2  :  (3  :  []))
foldr f z [1,2,3] = 1 `f` (2 `f` (3 `f` z ))
```

This leads to one of the most famous identities in Haskell: `foldr (:) [] xs =
xs`.  That's because if we go in and replace all `(:)`s with `(:)`, and replace
all `[]`s with `[]`... we get back the original list!

But something we can also do is give `foldr` a "custom cons".  A custom cons
that will go in place of the normal cons.

This problem is well-suited for such a custom cons: instead of normal `(:)`,
we'll write a custom cons that respects the rules of reaction: we can't have
two "anti-letters" next to each other:

```haskell
anti :: Char -> Char -> Bool
anti x y = toLower x == toLower y && x /= y

funkyCons :: Char -> String -> String
x `funkyCons` (y:xs)
    | anti x y  = xs
    | otherwise = x:y:xs
x `funkyCons` []     = [x]
```

So, `foldr funkyCons []` will go through a list and replace all `(:)` (cons)
with `funkyCons`, which will "bubble up" the reaction.

So, that's just the entire part 1!

```haskell
day05a :: String -> Int
day05a = length . foldr funkyCons []
```

For part 2 we can just find the minimum length after trying out every
character.

```haskell
day05b :: String -> Int
day05b xs = minimum [ length $ foldr funkyCons [] (remove c xs)
                    | c <- ['a' .. 'z']
                    ]
  where
    remove c = filter ((/= c) . toLower)
```

(Note that in the actual input, there is a trailing newline, so in practice we
have to strip it from the input.)

### Day 5 Benchmarks

#### Foldr method

```
>> Day 05a
benchmarking...
time                 5.609 ms   (5.555 ms .. 5.662 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 5.591 ms   (5.541 ms .. 5.655 ms)
std dev              166.2 μs   (109.9 μs .. 269.5 μs)
variance introduced by outliers: 12% (moderately inflated)

>> Day 05b
benchmarking...
time                 112.6 ms   (111.2 ms .. 115.7 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 111.8 ms   (111.3 ms .. 112.9 ms)
std dev              1.111 ms   (378.1 μs .. 1.713 ms)
variance introduced by outliers: 11% (moderately inflated)
```

#### Group homomorphism method

```
>> Day 05a
benchmarking...
time                 19.20 ms   (17.73 ms .. 21.02 ms)
                     0.978 R²   (0.965 R² .. 0.999 R²)
mean                 18.14 ms   (17.68 ms .. 18.83 ms)
std dev              1.326 ms   (730.9 μs .. 1.777 ms)
variance introduced by outliers: 30% (moderately inflated)

>> Day 05b
benchmarking...
time                 86.48 ms   (84.09 ms .. 88.22 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 87.50 ms   (86.39 ms .. 90.59 ms)
std dev              3.148 ms   (573.0 μs .. 5.100 ms)
```

Day 6
-----

*[Prompt][d06p]* / *[Code][d06g]* / *[Rendered][d06h]*

[d06p]: https://adventofcode.com/2018/day/6
[d06g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day06.hs
[d06h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day06.html

Day 6 Part 1 has us build a [Voronoi Diagram][], and inspect properties of it.
Again, it's all very functional already, since we just need, basically:

1.  A function to get a voronoi diagram from a set of points
2.  A function to query the diagram for properties we care about

[Voronoi Diagram]: https://en.wikipedia.org/wiki/Voronoi_diagram

Along the way, types will help us write our programs, because we constantly
will be asking the compiler for "what could go here" sort of things; it'll also
prevent us from putting the wrong pieces together!

We're going to leverage the *[linear][]* library again, for its `V2 Int` type
for our points.  It has a very useful `Num` and `Foldable` instance, which we
can use to write our `distance` function:

```haskell
type Point = V2 Int

distance :: Point -> Point -> Int
distance x y = sum $ abs (x - y)
```

We're going to be representing our voronoi diagram using a `Map Point Point`: a
map of points to the location of the "Site" they are assigned to.

We can generate such a map by getting a `Set Point` (a set of all points within
our area of interest) and using `M.fromSet :: (Point -> Point) -> Set Point ->
Map Point Point`, to assign a Site to each point.

First, we build a bounding box so don't need to generate an infinite map.  The
`boundingBox` function will take a non-empty list of points (from
`Data.List.NonEmpty`) and return a `V2 Point`, which the lower-left and
upper-right corners of our bounding box.

We need to iterate through the whole list and accumulate the minimum and
maximums of x and y.  We can do it all in one pass by taking advantage of the
`(Semigroup a, Semigroup b) => Semigroup (a, b)` instance, the `Min` and `Max`
newtype wrappers to give us the appropriate semigroups, and using `foldMap1 ::
Semigroup m => (a -> m) -> NonEmpty a -> m`:

```haskell
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Semigroup.Foldable

type Box = V2 Point

boundingBox :: NonEmpty Point -> Box
boundingBox ps = V2 xMin yMin `V2` V2 xMax yMax
  where
    (Min xMin, Min yMin, Max xMax, Max yMax) = flip foldMap1 ps $ \(V2 x y) ->
        (Min x, Min y, Max x, Max y)
```

(Note that we can just use `foldMap`, because `Min` and `Max` have a `Monoid`
instance because `Int` is bounded.  But that's no fun!  And besides, what if we
had used `Integer`?)

(Also note that this could potentially blow up the stack, because tuples in
Haskell are lazy.  If we cared about performance, we'd use a strict tuple type
instead of the lazy tuple.  In this case, since we only have on the order of a
few thousand points, it's not a huge deal)

Next, we write a function that, given a non-empty set of sites and a point we
wish to label, return the label (site location) of that point.

We do this by making a `NonEmpty (Point, Int)` `dists` that  pair up sites to
the distance between that site and the point.

We need now to find the *minimum* distance in that `NonEmpty`.  But not only
that, we need to find the *unique* minimum, or return `Nothing` if we don't
have a unique minimum.

To do this, we can use `NE.head . NE.groupWith1 snd . NE.sortWith snd`.  This
will sort the `NonEmpty` on the second item (the distance `Int`), which puts
all of the minimal distances in the front.  `NE.groupWith1 snd` will then group
together the pairs with matching distances, moving all of the minimal distance
to the first item in the list.  Then we use the total `NE.head` to get the
first item: the non-empty list with the minimal distances.

Then we can pattern match on `(closestSite, minDist) :| []` to prove that this
"first list" has exactly one item, so the minimum is unique.

```haskell
labelVoronoi
    :: NonEmpty Point     -- ^ set of sites
    -> Point              -- ^ point to label
    -> Maybe Point        -- ^ the label, if unique
labelVoronoi sites p = do
    (closestSite, _) :| [] <- Just
                            . NE.head
                            . NE.groupWith1 snd
                            . NE.sortWith snd
                            $ dists
    pure closestSite
  where
    dists                  = sites <&> \site -> (site, distance p site)
```

Once we have our voronoi diagram `Map Point Point` (map of points to
nearest-site locations), we can use our `freqs :: [Point] -> Map Point Int` function
that we've used many times to get a `Map Point Int`, or a map from Site points to
Frequencies --- essentially a map of Sites to the total area of the cells
assigned to them.  The problem asks us what the size of the largest cell is, so
that's the same as asking for the largest frequency, `maximum`.

```haskell
queryVoronoi :: Map Point Point -> Int
queryVeronoi = maximum . freqs . M.elems
```

One caveat: we need to ignore cells that are "infinite".
To that we can create the set of all Sitse that touch the border, and then
filter out all points in the map that are associated with a Site that touches
the border.

```haskell
cleanVoronoi :: Box -> Map Point Point -> Map Point Point
cleanVoronoi (V2 (V2 xMin yMin) (V2 xMax yMax)) voronoi =
                  M.filter (`S.notMember` edges) voronoi
  where
    edges = S.fromList
          . mapMaybe (\(point, site) -> site <$ guard (onEdge point))
          . M.toList
          $ voronoi
    onEdge (V2 x y) = or [ x == xMin, x == xMax, y == yMin, y == yMax ]
```

We turn `edges` into a `Set` (instead of just a list) because of the fast
`S.notMember` function, to check if a Site ID is in the set of edge-touching
ID's.


Finally, we need to get a function from a bounding box `Box` to `[Point]`: all
of the points in that bounding box.  Luckily, this is exactly what the `Ix`
instance of `V2 Int` gets us:

```haskell
import qualified Data.Ix as Ix

bbPoints :: Box -> [Point]
bbPoints (V2 mins maxs) = Ix.range (mins, maxs)
```

And so Part 1 is:

```haskell
day06a :: NonEmpty Point -> Int
day06a sites = queryVoronoi cleaned
  where
    bb      = boundingBox sites
    voronoi = catMaybes
            . M.fromSet (labelVoronoi sites)
            . S.fromList
            $ bbPoints bb
    cleaned = cleanVoronoi bb voronoi
```

Basically, a series of somewhat complex queries (translated straight from the
prompt) on a voronoi diagram generated by a set of points.

Part 2 is much simpler; it's just filtering for all the points that have a
given function, and then counting how many points there are.

```haskell
day06b :: NonEmpty Point -> Int
day06b sites = length
             . filter ((< 10000) . totalDist)
             . bbPoints
             . boundingBox
             $ sites
  where
    totalDist p = sum $ distance p <$> sites
```

1.  Get the bounding box with `boundingBox`
2.  Generate all of the points in that bounding box with `bbPoints`
3.  Filter those points for just those where their `totalDist` is less than
    10000
4.  Find the number of such points

Another situation where the Part 2 is much simpler than Part 1 :)

Our parser isn't too complicated; it's similar to the parsers from the previous
parts:

```haskell
parseLine :: String -> Maybe Point
parseLine = (packUp =<<)
          . traverse readMaybe
          . words
          . clearOut (not . isDigit)
  where
    packUp [x,y] = Just $ V2 x y
    packUp _     = Nothing
```

### Day 6 Benchmarks

```
>> Day 06a
benchmarking...
time                 580.9 ms   (505.9 ms .. 664.7 ms)
                     0.997 R²   (0.991 R² .. 1.000 R²)
mean                 562.6 ms   (548.9 ms .. 572.7 ms)
std dev              15.52 ms   (14.07 ms .. 17.69 ms)
variance introduced by outliers: 19% (moderately inflated)

>> Day 06b
benchmarking...
time                 100.2 ms   (99.16 ms .. 102.3 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 103.6 ms   (101.9 ms .. 108.9 ms)
std dev              4.418 ms   (1.537 ms .. 7.029 ms)
```

Day 7
-----

*[Prompt][d07p]* / *[Code][d07g]* / *[Rendered][d07h]*

[d07p]: https://adventofcode.com/2018/day/7
[d07g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day07.hs
[d07h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day07.html

Reflections to come soon!

### Day 7 Benchmarks

```
>> Day 07a
benchmarking...
time                 389.8 μs   (385.5 μs .. 397.7 μs)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 389.2 μs   (387.1 μs .. 392.9 μs)
std dev              9.506 μs   (3.827 μs .. 15.65 μs)
variance introduced by outliers: 16% (moderately inflated)

>> Day 07b
benchmarking...
time                 428.1 μs   (426.8 μs .. 431.3 μs)
                     0.999 R²   (0.995 R² .. 1.000 R²)
mean                 430.4 μs   (427.6 μs .. 441.3 μs)
std dev              16.14 μs   (3.897 μs .. 33.35 μs)
variance introduced by outliers: 31% (moderately inflated)
```

Day 8
-----

*[Prompt][d08p]* / *[Code][d08g]* / *[Rendered][d08h]*

[d08p]: https://adventofcode.com/2018/day/8
[d08g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day08.hs
[d08h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day08.html

Another nice one for Haskell!  We're just parsing a stream of `Int`s here :)

```haskell
import qualified Text.Parsec    as P

type Parser = P.Parsec [Int] ()
```

with a `Parsec [Int] ()`, it means that our "tokens" are `Int`.  That means
`P.anyToken :: Parser Int` will pop the next `Int` from the stream.

Our Day 1 will be the `sum1`, which will parse a stream of `Int`s into the sum
of all the metadatas.

```haskell
sum1 :: Parser Int
sum1 = do
    numChild <- P.anyToken
    numMeta  <- P.anyToken
    childs   <- sum <$> replicateM numChild sum1
    metas    <- sum <$> replicateM numMeta  P.anyToken
    pure $ childs + metas
```

And so part 1 is:

```haskell
day01a :: [Int] -> Int
day01a xs = fromRight 0 . P.parse sum1 ""
```

Part 2 is similar.  Again, we parse a stream of ints into a sum:

```
sum2 :: Parser Int
sum2 = do
    numChild <- P.anyToken
    numMeta  <- P.anyToken
    childs   <- replicateM numChild sum2
    metas    <- replicateM numMeta  P.anyToken
    pure $ if null childs
      then sum metas
      else sum . mapMaybe (\i -> childs ^? ix (i - 1)) $ metas
```

I'm using `xs ^? ix i` (from lens) as a "safe indexing", that returns `Maybe
a`.  We need to remember to index into `i - 1` because our indexing starts at
one!

And so part 2 is:

```haskell
day02a :: [Int] -> Int
day02a = fromRight 0 . P.parse sum1 ""
```

We can get a list of `[Int]` from a string input using `map read . words`.

### Day 8 Benchmarks

```
>> Day 08a
benchmarking...
time                 36.80 ms   (33.65 ms .. 42.18 ms)
                     0.963 R²   (0.921 R² .. 1.000 R²)
mean                 35.47 ms   (34.29 ms .. 38.27 ms)
std dev              3.477 ms   (561.7 μs .. 5.309 ms)
variance introduced by outliers: 36% (moderately inflated)

>> Day 08b
benchmarking...
time                 29.27 ms   (28.34 ms .. 32.94 ms)
                     0.919 R²   (0.787 R² .. 1.000 R²)
mean                 29.46 ms   (28.41 ms .. 33.59 ms)
std dev              4.133 ms   (89.00 μs .. 7.910 ms)
variance introduced by outliers: 57% (severely inflated)
```

Day 9
-----

*[Prompt][d09p]* / *[Code][d09g]* / *[Rendered][d09h]*

[d09p]: https://adventofcode.com/2018/day/9
[d09g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day09.hs
[d09h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day09.html

And today features the re-introduction of an Advent of Code staple: the
(circular) tape/zipper!  I used this data structure last year for days 5, 17,
18 and 23, and I consider them near and dear to my heart as Advent of Code data
structures :)

Last year, I wrote my own implementations on the spot, but since then I've come
to appreciate the *[pointed-list][]* library.  A circular tape is a circular
data structure with a "focus" that you can move back and forth in.  This is
the data structure that implements exactly what the challenge talks about!
It's linear-time on "moving the focus", and constant-time on insertions and
deletions.

[pointed-list]: https://hackage.haskell.org/package/pointedlist

The center of everything is the `place` function, which takes a number to place
and a tape to place it in, and returns an updated tape with the "score"
accumulated for that round.

We see that it is mostly a straightforward translation of the problem
statement.  If `x` is a multiple of 23, then we move 7 spaces to the left, and
return the resulting tape with the item deleted.  The score is the deleted item
plus `x`.  Otherwise, we just move 2 spaces to the right and insert `x`, with a
score of 0.

```haskell
place
    :: Int                       -- ^ number to place
    -> PointedList Int           -- ^ tape
    -> (Int, PointedList Int)    -- ^ resulting tape, and scored points
place x l
    | x `mod` 23 == 0
    = let l'       = PL.moveN (-7) l
          toAdd    = _focus l'
      in  (toAdd + x, fromJust (PL.deleteRight l'))
    | otherwise
    = (0, (PL.insertLeft x . PL.moveN 2) l)
```

We wrap it all up with a `run` function, which is a strict fold over a list of
`(currentPlayer, itemToPlace)` pairs, accumulating a `(scorecard, tape)` state
(our scorecard will be a vector where each index is a different player's
score). At each step, we `place`, and use the result to update our scorecard
and tape. The *lens* library offers some nice tool for incrementing a given
index of a vector.

```haskell
run
    :: Int                  -- ^ number of players
    -> Int                  -- ^ Max # of piece
    -> V.Vector Int
run numPlayers maxPiece = fst
                        . foldl' go (V.replicate numPlayers 0, PL.singleton 0)
                        $ zip players toInsert
  where
    go (!scores, !tp) (!player, !x) = (scores & ix player +~ pts, tp')
      where
        (pts, tp') = place x tp
    players  = (`mod` numPlayers) <$> [0 ..]
    toInsert = [1..maxPiece]
```

And that's it!  The answer is just the maximal score in the final score vector:

```haskell
day09a :: Int -> Int -> Int
day09a numPlayers maxPiece = V.maximum (run numPlayers maxPiece)

day09b :: Int -> Int -> Int
day09b numPlayers maxPiece = V.maximum (run numPlayers (maxPiece * 100))
```

From this naive implementation, Part 1 takes 56.ms, and Part 2 takes 4.5s.

### Day 9 Benchmarks

```
>> Day 09a
benchmarking...
time                 55.91 ms   (53.86 ms .. 57.87 ms)
                     0.997 R²   (0.991 R² .. 1.000 R²)
mean                 55.11 ms   (54.35 ms .. 56.27 ms)
std dev              1.746 ms   (978.3 μs .. 2.493 ms)

>> Day 09b
benchmarking...
time                 4.563 s    (4.318 s .. 4.792 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.790 s    (4.675 s .. 4.864 s)
std dev              113.2 ms   (33.28 ms .. 149.6 ms)
variance introduced by outliers: 19% (moderately inflated)
```

Day 10
------

*[Prompt][d10p]* / *[Code][d10g]* / *[Rendered][d10h]* / *[Blog][d10b]*

[d10p]: https://adventofcode.com/2018/day/10
[d10g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day10.hs
[d10h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day10.html
[d10b]: https://blog.jle.im/entry/shifting-the-stars.html

I originally did this by running a simulation, parting the velocity and points
into two lists and using `zipWith (+)` for the simulation.  However, I found a
much nicer closed-form version that [I wrote about in my blog][d10b]!

### Day 10 Benchmarks

```
>> Day 10a
benchmarking...
time                 8.260 ms   (8.109 ms .. 8.399 ms)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 8.271 ms   (8.197 ms .. 8.361 ms)
std dev              219.6 μs   (135.5 μs .. 344.9 μs)

>> Day 10b
benchmarking...
time                 5.800 ms   (5.769 ms .. 5.846 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 5.779 ms   (5.760 ms .. 5.800 ms)
std dev              58.91 μs   (43.82 μs .. 90.77 μs)
```

Day 11
------

*[Prompt][d11p]* / *[Code][d11g]* / *[Rendered][d11h]*

[d11p]: https://adventofcode.com/2018/day/11
[d11g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day11.hs
[d11h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day11.html

Day 11 is a nice opportunity to demonstrate dynamic programming in a purely
functional language like Haskell.

Once we define a function to get a power level based on a serial number:

```haskell
type Point = V2 Int

powerLevel :: Int -> Point -> Int
powerLevel sid (V2 x y) = hun ((rid * y + sid) * rid) - 5
  where
    hun = (`mod` 10) . (`div` 100)
    rid = x + 10
```

We can create a `Map` of of `Point` to power level, by creating the set of all
points (using `range` from *Data.Ix*) and using `M.fromSet` with a function.

```haskell
mkMap :: Int -> Map Point Int
mkMap i = M.fromSet (powerLevel i)
        . S.fromList
        $ range (V2 1 1, V2 300 300)
```

Now, both Part 1 and Part 2 involve finding sums of contiguous squares in the
input.  One popular way to do this quickly for many different sums is to build
a [summed-area table][]

```haskell
summedAreaTable :: Map Point Int -> Map Point Int
summedAreaTable mp = force sat
  where
    sat = M.mapWithKey go mp
    go p0 v = (+ v) . sum . catMaybes $
      [ negate <$> M.lookup (p0 - V2 1 1) sat
      ,            M.lookup (p0 - V2 1 0) sat
      ,            M.lookup (p0 - V2 0 1) sat
      ]
```

This is where the dynamic programming happens: our summed area is `sat`, and we
define `sat` in a self-recursive way, using `M.mapWithKey go`.  `M.mapWithKey
go` lazily generates each cell of `sat` by *referring to other cells in `sat`*.
Because of laziness, `mapWithKey` doesn't do any actual "mapping"; but, rather,
allocates thunks at each value in the map.  As soon as these thunks are asked
for, they resolve and are kept as resolved values.

For example, note that `go (V2 1 1) v11` does not refer to any other value.  So,
the map at `V2 1 1` is just `v11`.

However, `go (V2 2 1) v21` depends on one other value: `M.lookup (V2 1 1) sat`.
But, because we already have evaluated this to `v11`, all is well; our answer
is `v21 + v11`.

Now, `go (V2 2 2) v22` depends on three other values: it depends on `M.lookup (V
1 1) sat`, `M.lookup (V2 1 2) sat`, and `M.lookup (V2 1 2) sat`.  GHC will go
and evaluate the ones it needs to evaluate, caching them in the values of the
map, and then just now return the pre-evaluated results.

In this way, we build the summed area table "lazily" in a self-recursive way.
At the end of it all, we return `force sat`, which makes sure the entire `sat`
map is filled out all the way (getting rid of all thunks) when the user
actually tries to *use* the summed area table.

The rest of this involves just making a list of all possible sums of squares,
and finding the maximum of all of them.  Because all of our sums of squares are
now calculable in O(1) on the size of the square (after we generate our
table), the search is very manageable.

```haskell
fromSAT :: Map Point Int -> Point -> Int -> Int
fromSAT sat (subtract (V2 1 1)->p) n = sum . catMaybes $
    [            M.lookup p            sat
    ,            M.lookup (p + V2 n n) sat
    , negate <$> M.lookup (p + V2 0 n) sat
    , negate <$> M.lookup (p + V2 n 0) sat
    ]

findMaxAny :: Map Point Int -> (Point, Int)
findMaxAny mp = fst . maximumBy (comparing snd)
             $ [ ((p, n), fromSAT sat p n)
               , n <- [1 .. 300]
               , p <- range (V2 1 1, V2 (300 - n + 1) (300 - n + 1))
               ]
  where
    sat = summedAreaTable mp
```

### Day 11 Benchmarks

Note these benchmarks are actually using an early-cut-off version of
`findMaxAny` that I implemented after thinking about ways of optimization.

```
>> Day 11a
benchmarking...
time                 117.5 ms   (113.8 ms .. 120.1 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 119.5 ms   (117.4 ms .. 124.3 ms)
std dev              4.802 ms   (2.094 ms .. 7.573 ms)
variance introduced by outliers: 11% (moderately inflated)

>> Day 11b
benchmarking...
time                 1.115 s    (1.074 s .. 1.176 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.138 s    (1.120 s .. 1.165 s)
std dev              27.13 ms   (4.430 ms .. 35.08 ms)
variance introduced by outliers: 19% (moderately inflated)
```

Day 12
------

*[Prompt][d12p]* / *[Code][d12g]* / *[Rendered][d12h]*

[d12p]: https://adventofcode.com/2018/day/12
[d12g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day12.hs
[d12h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day12.html

Day 12 is made a little more fun with everyone's favorite Haskell data
structures: maps and sets! (Note that I've pretty much used Maps and Sets for
every challenge, more or less!)

We can represent a "context", or neighborhood, as a `Set (Finite 5)`, where
`Finite 5` can be thought of as a type that only contains the numbers 0, 1, 2,
3, and 4 (five elements only).  We'll treat 0 as "two to the left", 1 as "one
to the left", 2 as "the current point", 3 as "one to the right", and 4 as "two
to the right".  The set will *contain* the given finite if it is "on" in that
position.  So, for example, the context `#.##.` would be `S.fromList [0,2,3]`.

```haskell
type Ctx = Set (Finite 5)
```

Our ruleset will be `Set Ctx`, or a set of neighborhoods.  If a given
neighborhood is *in* the set, then that means that the plant is meant to turn
on.  Otherwise, it means that the plant is meant to turn off.  So, `#.##. => #`
would mean that the item `S.fromList [0,2,3]` is in the ruleset, but `##..# =>
.` would mean that the item `S.fromList [0,1,4]` is *not* in the ruleset.

Finally, the type of our "world" is just `Set Int`.  If a plant is "on", then
its index will be in the set.  Otherwise, its index will *not* be in the set.

One nice thing about representing the world as `Set Int` is that getting the
"sum of all plant IDs that are on" is just `sum :: Set Int -> Int` :)

Writing our step function is going to be filtering all of the "candidate"
positions for the ones that remain "on".  That's it!  We perform this filter by
aggregating the neighborhood around each point and checking if the neighborhood
is in the ruleset.

```haskell
step
    :: Set Ctx
    -> Set Int
    -> Set Int
step ctxs w0 = S.fromDistinctAscList
             . filter go
             $ [S.findMin w0 - 2 .. S.findMax w0 + 2]
  where
    go i = neighbs `S.member` ctxs
      where
        neighbs = S.fromDistinctAscList . flip filter finites $ \j ->
          (i - 2 + fromIntegral j) `S.member` w0
```

Part 2 requires a bit of trickery.  If we monitor our outputs, we can observe
that the entire shape of the world starts to loop after a given amount of time.
We can find this loop structure by stepping repeatedly and finding the first
item that is repeated, by using a "seen items" set.  We have to make sure to
"normalize" our representation so that the same shame will be matched no matter
what coordinate it starts at.  I did this by subtracting out the minimum item
in the set, so that the leftmost plant is always at zero.

```haskell
findLoop
    :: Set Ctx
    -> Set Pos
    -> (Int, Int, Int)      -- time to loop, loop size, loop incr
findLoop ctxs w0 = go (M.singleton w0 (0, 0)) 1 w0
  where
    go !seen !i !w = case M.lookup w'Norm seen of
        Nothing              -> go (M.insert w'Norm (mn, i) seen) (i + 1) w'
        Just (seenMn, seenI) -> (seenI, i - seenI, mn - seenMn)
      where
        w'           = step ctxs w
        (mn, w'Norm) = normalize w'
    normalize w = (mn, S.map (subtract mn) w)
      where
        mn = S.findMin w
```

And now we can be a little clever using `divMod` to factor out 50 billion into
the "initialization", the "loop amount", and the "amount to increase":

```haskell
stepN
    :: Int
    -> Set Pos
    -> Set Ctx
    -> Set Pos
stepN n w ctx = goN extra
              . S.map (+ (loopIncr * looped))
              . goN ttl
              $ w
  where
    goN m = (!!! m) . iterate (step ctx)
    (ttl, loopSize, loopIncr) = findLoop ctx w
    (looped, extra) = (n - ttl) `divMod` loopSize
```

### Day 12 Benchmarks

```haskell
>> Day 12a
benchmarking...
time                 1.603 ms   (1.586 ms .. 1.623 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.628 ms   (1.605 ms .. 1.689 ms)
std dev              110.9 μs   (34.20 μs .. 210.2 μs)
variance introduced by outliers: 51% (severely inflated)

>> Day 12b
benchmarking...
time                 33.39 ms   (32.78 ms .. 34.02 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 33.83 ms   (33.43 ms .. 35.01 ms)
std dev              1.143 ms   (69.18 μs .. 1.958 ms)
variance introduced by outliers: 11% (moderately inflated)
```

Day 13
------

*[Prompt][d13p]* / *[Code][d13g]* / *[Rendered][d13h]*

[d13p]: https://adventofcode.com/2018/day/13
[d13g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day13.hs
[d13h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day13.html

### Day 13 Benchmarks

```
>> Day 13a
benchmarking...
time                 16.18 ms   (16.10 ms .. 16.27 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 16.11 ms   (16.07 ms .. 16.15 ms)
std dev              100.8 μs   (73.57 μs .. 145.0 μs)

>> Day 13b
benchmarking...
time                 26.35 ms   (25.68 ms .. 26.87 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 26.78 ms   (26.39 ms .. 27.28 ms)
std dev              995.5 μs   (671.4 μs .. 1.251 ms)
variance introduced by outliers: 10% (moderately inflated)
```
