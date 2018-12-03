Reflections
===========

[Table of Contents][]

[Table of Contents]: https://github.com/mstksg/advent-of-code-2018#reflections-and-benchmarks

Day 1
-----

*[Code][d01g]* / *[Rendered][d01h]*

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

*[Code][d02g]* / *[Rendered][d02h]*

[d02g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC2018/Challenge/Day02.hs
[d02h]: https://mstksg.github.io/advent-of-code-2018/src/AOC2018.Challenge.Day02.html

Day 2 part 1 works out nicely in a functional paradigm because it's just a
couple of frequency tables.

I often use this function to generate a frequency table of values in a list:

```haskell
import qualified Data.Map as M

freqs :: [a] -> Map a Int
freqs = M.fromListWith (+) . map (,1)
```

Day 2 part 1 is then to just:

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

*[Code][d03g]* / *[Rendered][d03h]*

[d03g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC2018/Challenge/Day03.hs
[d03h]: https://mstksg.github.io/advent-of-code-2018/src/AOC2018.Challenge.Day03.html

Day 3 brings back one of my favorite data structures in Haskell -- `Map (Int,
Int)`!  It's basically a sparse grid.  It maps coordinates to values at each
coordinate.

We're going to use `V2 Int` (from *linear*) instead of `(Int, Int)` (they're
the same thing), because get to use the overloaded `+` operator to do
point-wise addition.  Let's also define a rectangle specification record type
to keep things clean:

```haskell
type Coord = V2 Int

data Rect = R { rStart :: Coord
              , rSize  :: Coord
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

From there, we need to just count how many frequencies we observe are greater
than 1.  We can do that by filtering and counting how many are left.

```haskell
import qualified Data.Map as M

day03a :: [Rect] -> Int
day03a = length . filter (>= 2) . M.elems . layTiles
```

For `day03`, we can just use `find` to search our list of claims by id's,
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
day03b :: [(Int, Rect)] -> Maybe Int
day03b ts = fst <$> find (noOverlap stakes . snd) ts
  where
    stakes = layTiles (map snd ts)
```

Parsing for this one is a little tricky, but we can get away with just clearing
out all non-digit characters and using `words` to split up a string into its
constituent words, and `readMaybe` to read each one.

```haskell
parseLine :: String -> Maybe (Int, Rect)
parseLine = mkLine
          . mapMaybe readMaybe
          . words
          . map onlyDigits
  where
    mkLine [i,x0,y0,w,h] = Just (i, R (V2 x0 y0) (V2 w h))
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
