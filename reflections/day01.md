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
