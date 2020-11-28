Day 13 is fun because it can be stated in terms of a *[hylomorphism][]*!

[hylomorphism]: https://en.wikipedia.org/wiki/Hylomorphism_(computer_science)

First, our data types:

```haskell
type Point = V2 Int

data Turn = TurnNW      -- ^ a forward-slash mirror @/@
          | TurnNE      -- ^ a backwards-slash mirror @\\@
          | TurnInter   -- ^ a four-way intersection
  deriving (Eq, Show, Ord)

data Dir = DN | DE | DS | DW
  deriving (Eq, Show, Ord, Enum, Bounded)

data Cart = C { _cDir   :: Dir
              , _cTurns :: Int
              }
  deriving (Eq, Show)

makeLenses ''Cart

newtype ScanPoint = SP { _getSP :: Point }
  deriving (Eq, Show, Num)

instance Ord ScanPoint where
    compare = comparing (view _y . _getSP)
           <> comparing (view _x . _getSP)

type World = Map Point     Turn
type Carts = Map ScanPoint Cart
```

We will be using `Map ScanPoint Cart` as our priority queue; `ScanPoint`
newtype-wraps a `Point` in a way that its `Ord` instance will give us the
lowest `y` first, *then* the lowest `x` to break ties.

Note that we don't ever have to store any of the "track" positions, `|` or `-`.
That's because they don't affect the carts in any way.

Next, we can implement the actual logic of moving a single `Cart`:

```haskell
stepCart :: World -> ScanPoint -> Cart -> (ScanPoint, Cart)
stepCart w (SP p) c = (SP p', maybe id turner (M.lookup p' w) c)
  where
    p' = p + case c ^. cDir of
      DN -> V2 0    (-1)
      DE -> V2 1    0
      DS -> V2 0    1
      DW -> V2 (-1) 0
    turner = \case
      TurnNW    -> over cDir $ \case DN -> DE; DE -> DN; DS -> DW; DW -> DS
      TurnNE    -> over cDir $ \case DN -> DW; DW -> DN; DS -> DE; DE -> DS
      TurnInter -> over cTurns (+ 1) . over cDir (turnWith (c ^. cTurns))
    turnWith i = case i `mod` 3 of
      0 -> turnLeft
      1 -> id
      _ -> turnLeft . turnLeft . turnLeft
    turnLeft DN = DW
    turnLeft DE = DN
    turnLeft DS = DE
    turnLeft DW = DS
```

There are ways we can the turning and `Dir` manipulations, but this way already
is pretty clean, I think!  We use lens combinators like `over` to simplify our
updating of carts.  If there is no turn at a given coordinate, then the cart
just stays the same, and only the position updates.

Now, to separate out the *running* of the simulation from the *consumption* of
the results, we can make a type that emits the result of a single step in the
world:

```haskell
data CartLog a = CLCrash Point a      -- ^ A crash, at a given point
               | CLTick        a      -- ^ No crashes, just a normal timestep
               | CLDone  Point        -- ^ Only one car left, at a given point
  deriving (Show, Functor)
```

And we can use that to implement `stepCarts`, which takes a "waiting, done"
queue of carts and:

1.  If `waiting` is empty, we dump `done` back into `waiting` and emit `CLTick`
    with our updated state.  However, if `done` is empty, then we are done;
    emit `CLDone` with no new state.
2.  Otherwise, pop an cart from `waiting` and move it.  If there is a crash,
    emit `CLCrash` with the updated state (with things deleted).

```haskell
stepCarts
    :: World
    -> (Carts, Carts)
    -> CartLog (Carts, Carts)
stepCarts w (waiting, done) = case M.minViewWithKey waiting of
    Nothing -> case M.minViewWithKey done of
      Just ((SP lastPos, _), M.null->True) -> CLDone lastPos
      _                                    -> CLTick (done, M.empty)
    Just (uncurry (stepCart w) -> (p, c), waiting') ->
      case M.lookup p (waiting' <> done) of
        Nothing -> CLTick             (waiting'           , M.insert p c done)
        Just _  -> CLCrash (_getSP p) (M.delete p waiting', M.delete p done  )
```

Now, we can write our consumers.  These will be fed the results of `stepCarts`
as they are produced.  However, the `a` parameters will actually be the "next
results", in a way:

```haskell
-- | Get the result of the first crash.
firstCrash :: CartLog (Maybe Point) -> Maybe Point
firstCrash (CLCrash p _) = Just p   -- this is it, chief
firstCrash (CLTick    p) = p        -- no, we have to go deeper
firstCrash (CLDone  _  ) = Nothing  -- we reached the end of the line, no crash.

-- | Get the final point.
lastPoint :: CartLog Point -> Point
lastPoint (CLCrash _ p) = p   -- we have to go deeper
lastPoint (CLTick    p) = p   -- even deeper
lastPoint (CLDone  p  ) = p   -- we're here
```

And now:

```haskell
day13a :: World -> Carts -> Maybe Point
day13a w c = (firstCrash `hylo` stepCarts w) (c, M.empty)

day13b :: World -> Carts -> Point
day13b w c = (lastPoint `hylo` stepCarts w) (c, M.empty)
```

The magic of `hylo` is that, as `firstCrash` and `lastPoint` "demand" new
values or points, `hylo` will ask `stepCarts w` for them.  So, `stepCarts w` is
iterated as many times as `firstCrash` and `lastPoint` needs.
