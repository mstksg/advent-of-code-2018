Reflections
===========

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
      | x `S.member` seen = Just x                      -- this is it
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

Day 1 part 1 is then to just:

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
day02a = fmap (uncurry onlySame)
       . firstNeighbor
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

### Day 3 Benchmarks

```
>> Day 03a
benchmarking...
time                 489.7 ms   (423.5 ms .. 552.5 ms)
                     0.998 R²   (0.991 R² .. 1.000 R²)
mean                 500.4 ms   (490.2 ms .. 510.6 ms)
std dev              11.81 ms   (10.13 ms .. 12.69 ms)
variance introduced by outliers: 19% (moderately inflated)

>> Day 03b
benchmarking...
time                 474.4 ms   (442.9 ms .. 516.5 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 474.9 ms   (468.3 ms .. 478.3 ms)
std dev              6.355 ms   (517.1 μs .. 7.878 ms)
variance introduced by outliers: 19% (moderately inflated)
```
