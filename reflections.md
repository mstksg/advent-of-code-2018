Reflections
===========

[Table of Contents][]

[Table of Contents]: https://github.com/mstksg/advent-of-code-2018#reflections-and-benchmarks

Day 1
-----

*[Prompt][d01p]* / *[Code][d01g]* / *[Rendered][d01h]*

[d01p]: https://adventofcode.com/2018/day/1
[d01g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC2018/Challenge/Day01.hs
[d01h]: https://mstksg.github.io/advent-of-code-2018/src/AOC2018.Challenge.Day01.html

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
[d02g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC2018/Challenge/Day02.hs
[d02h]: https://mstksg.github.io/advent-of-code-2018/src/AOC2018.Challenge.Day02.html

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
[d03g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC2018/Challenge/Day03.hs
[d03h]: https://mstksg.github.io/advent-of-code-2018/src/AOC2018.Challenge.Day03.html

Day 3 brings back one of my favorite data structures in Haskell -- `Map (Int,
Int)`!  It's basically a sparse grid.  It maps coordinates to values at each
coordinate.

We're going to use `V2 Int` (from *linear*) instead of `(Int, Int)` (they're
the same thing), because we get to use the overloaded `+` operator to do
point-wise addition.  Let's also define a rectangle specification and claim
record type to keep things clean:

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
[d04g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC2018/Challenge/Day04.hs
[d04h]: https://mstksg.github.io/advent-of-code-2018/src/AOC2018.Challenge.Day04.html

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

*[Prompt][d05p]* / *[Code][d05g]* / *[Rendered][d05h]*

[d05p]: https://adventofcode.com/2018/day/5
[d05g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC2018/Challenge/Day05.hs
[d05h]: https://mstksg.github.io/advent-of-code-2018/src/AOC2018.Challenge.Day05.html

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

### Alternative implementation

One interesting thing we can notice is that the problem exactly describes the
behavior of a [Free Group][], from group theory (generated by the set of the 26
letters of the alphabet).  Basically we can talk about reducing the string
`dabAcCaCBAcCcaDA` as the result of `d <> a <> b <> A <> c <> C <> a <> C <> B
<> ...` etc., where `<>` is the group action, and `A` stands for "`a` inverse".

[Free Group]: https://en.wikipedia.org/wiki/Free_group

So we'll say that we're working over the free group generated by
`"abcdefghijklmnopqrstuvwxyz"`, and we'll interpret a capital letter as the
group inverse of the lowercase component.  We'll interpret any non-alphabet
characters as just the identity element.

We can use *[Data.Group.Free][]* from the *[free-algebras][]* library, which
offers a free group type `FreeGroupL`, to do so:

[free-algebras]: https://hackage.haskell.org/package/free-algebras
[Data.Group.Free]: https://hackage.haskell.org/package/free-algebras/docs/Data-Group-Free.html

```haskell
import qualified Data.Group.Free as FG

inject :: Char -> FG.FreeGroupL Char
inject c
    | isAlpha c && isLower c = returnFree c
    | isAlpha c && isUpper c = invert $ returnFree c
    | otherwise              = identity
```

The free group has a "length" property (the length of the string of
valid combinations of generators), which we can access using `length .
FG.toList`.  So that makes Part 1 simply:

```haskell
day05a :: String -> Int
day05a = length . FG.toList . foldMap inject
```

Part 2 also models something direct from group theory: it's a *group
homomorphism*.  That is, it maps *F(26)* (the free group on the 26 letters of
the alphabet) to *F(25)* (the free group on the letters of the alphabet not
including our cleaned letter).

Luckily, the free group *F(S)* comes equipped with a free group homomorphism:

```haskell
foldMapFree
    :: Group b
    => (a -> b)
    -> (FreeGroupL a -> b)     -- the group homomorphism
```

That is, given any `a -> b` for a group `b`, we get a group homormohpsim from
`FreeGroup a` to `b`.  We can write a function from the *generators* of our
group (in this case, `Char`), and i'll give us a group homomorphism on the
*free group* on our generators.

We can use this to write our `F(26)` to `F(25)` group homomorphism:

```haskell
foldMapFree
    :: (Char -> FreeGroupL Char)  -- map letter sto letters-minus-some-letter
    -> FreeGroupL Char
    -> FreeGroupL Char
```

(Note that this type signature looks a lot like `=<<`, for monads.
Coincidence?)

We can now create a "cleaned" version of our reaction by using:

```haskell
clean :: Char -> FreeGroupL Char -> FreeGroupL Char
clean c = foldMapFree $ \d -> if d == c then mempty else returnFree d
           -- mempty is the identity element ^
```

And so that's part 2:

```haskell
part2 (foldMap inject -> xs) = minimum [ lenght $ G.toList (clean c xs)
                                       | c <- ['a' .. 'z']
                                       ]
```

Basically, we find the minimum of all of the possible "cleaned" lengths.

Note that we actually introduced a large optimization over the naive foldr
method, by accident: due to group theory, we know that for a group
homomorphism, "aggregating then cleaning" is the same as "cleaning then
aggregating".

This means that we are free to either "clean first, then aggregate", or
"aggregate first, then clean".  This choice is not obvious from just reading
the problem alone, and, indeed, it seems like the problem might be deliberately
written to obscure this choice from us.

But, thanks to group theory, we know that these are equivalent, so we can
substitute which ever version is more efficient!

This is, I believe, at the heart of what people say is the advantage of "using
monoids", "using monads", "using functors", etc. in Haskell.  That's because if
we state our programs in terms of monoids, monads, groups, functors, etc., then
we get *the entire body of group theory* (or monad theory, or functor theory,
etc.) to help us make program reductions that aren't immediately obviously
legal but that have already been proven to be equivalent by mathematicians.  We
hijack their work!

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
