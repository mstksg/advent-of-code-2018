This one feels complex at first (a generate-check-generate-check loop)...if you
take a generate-check loop, you also have to be sure to make sure you check the
case of 1 or 2 added digits.

However, it becomes much simpler if you separate the act of generation and
checking as two different things.  Luckily, with Haskell, this is fairly easy
with lazily linked lists.

```haskell
chocolatePractice :: [Int]
chocolatePractice = 3 : 7 : go 0 1 (Seq.fromList [3,7])
  where
    go !p1 !p2 !tp = newDigits ++ go p1' p2' tp'
      where
        sc1 = tp `Seq.index` p1
        sc2 = tp `Seq.index` p2
        newDigits = digitize $ sc1 + sc2
        tp' = tp <> Seq.fromList newDigits
        p1' = (p1 + sc1 + 1) `mod` length tp'
        p2' = (p2 + sc2 + 1) `mod` length tp'

digitize :: Int -> [Int]
digitize ((`divMod` 10)->(x,y))
    | x == 0    = [y]
    | otherwise = [x,y]
```

We use `go` to lazily generate new items as they are demanded.  Once the user
consumes all of the `newDigits` asks for more, `go` will be asked to generate
new digits.  The important thing is that this is demand-driven.

We keep track of the current tape using `Seq` from *Data.Sequence* for its O(1)
appends and O(log) indexing -- the two things we do the most.  We could also
get away with pre-allocation with vectors for amortized O(1) suffix appends and
O(1) indexing, as well.

Note that `chocolatePractice` is effectively the same for every per-user input
data. It's just a (lazily generated) list of all of the chocolate practice digits.

Part 1 then is just a `drop` then a `take`:

```haskell
day14a :: Int -> [Int]
day14a n = take 10 (drop n chocolatePractice)
```

Part 2, we can use `isPrefixOf` from *Data.List* and check every `tails` until
we get one that *does* have our digit list as a prefix:

```haskell
substrLoc :: [Int] -> [Int] -> Maybe Int
substrLoc xs = length
             . takeWhile (not . (xs `isPrefixOf`))
             . tails

day14b :: [Int] -> [Int]
day14b xs = xs `substrLoc` cholcatePractice
```

Note that `chocolatePractice` is essentially just a futumorphism, so this whole
thing can be stated in terms of a chronomorphism.  I don't know if there would
be any advantage in doing so.  But it's interesting to me that I solved Day 13
using a hylomorphism, and now Day 14 using what is essentially a chronomorphism
... so maybe recursion-schemes is the killer app for Advent of Code? :)

A note on benchmarks -- it's very difficult to benchmark Day 14, because I
couldn't get ghc to stop memoizing `chocolatePractice`.  This means my repeated
benchmarks kept on re-using the stored list.

However, using `time`, I timed Part 1 to about 180ms, and Part 2 to 10s.
