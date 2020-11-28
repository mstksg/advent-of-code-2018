Another nice one for Haskell!  We're just parsing a stream of `Int`s here :)

```haskell
import qualified Text.Parsec    as P

type Parser = P.Parsec [Int] ()
```

with a `Parsec [Int] ()`, it means that our "tokens" are `Int`.  That means
`P.anyToken :: Parser Int` will pop the next `Int` from the stream.

Our Day 1 will be the `sum1`, which will parse a stream of `Int`s into the sum
of all the metadatas.

```haskell
sum1 :: Parser Int
sum1 = do
    numChild <- P.anyToken
    numMeta  <- P.anyToken
    childs   <- sum <$> replicateM numChild sum1
    metas    <- sum <$> replicateM numMeta  P.anyToken
    pure $ childs + metas
```

And so part 1 is:

```haskell
day01a :: [Int] -> Int
day01a xs = fromRight 0 . P.parse sum1 ""
```

Part 2 is similar.  Again, we parse a stream of ints into a sum:

```
sum2 :: Parser Int
sum2 = do
    numChild <- P.anyToken
    numMeta  <- P.anyToken
    childs   <- replicateM numChild sum2
    metas    <- replicateM numMeta  P.anyToken
    pure $ if null childs
      then sum metas
      else sum . mapMaybe (\i -> childs ^? ix (i - 1)) $ metas
```

I'm using `xs ^? ix i` (from lens) as a "safe indexing", that returns `Maybe
a`.  We need to remember to index into `i - 1` because our indexing starts at
one!

And so part 2 is:

```haskell
day02a :: [Int] -> Int
day02a = fromRight 0 . P.parse sum1 ""
```

We can get a list of `[Int]` from a string input using `map read . words`.
