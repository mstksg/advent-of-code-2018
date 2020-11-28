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
