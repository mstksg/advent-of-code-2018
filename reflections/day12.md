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
