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

Note the benchmarks below are actually using an early-cut-off version of
`findMaxAny` that I implemented after thinking about ways of optimization.
