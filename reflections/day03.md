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
