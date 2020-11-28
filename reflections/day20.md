Like Day 4, this one is made pretty simple with parser combinators! :D

Just for clarity, we will tokenize the stream first -- but it's not strictly
necessary.

```haskell
data Dir = DN | DE | DS | DW
  deriving (Show, Eq, Ord)

data RegTok = RTStart
            | RTDir Dir
            | RTRParen
            | RTOr
            | RTLParen
            | RTEnd
  deriving (Show, Eq, Ord)

parseToks :: String -> [RegTok]
parseToks = mapMaybe $ \case
    '^' -> Just RTStart
    'N' -> Just $ RTDir DN
    'E' -> Just $ RTDir DE
    'W' -> Just $ RTDir DW
    'S' -> Just $ RTDir DS
    '|' -> Just RTOr
    '(' -> Just RTRParen
    ')' -> Just RTLParen
    '$' -> Just RTEnd
    _   -> Nothing
```

Now, to write our parser!  We will parse our `[RegTok]` stream into a set of
edges.

```haskell
import           Linear (V2(..))
import qualified Text.Parsec as P

-- V2 Int = (Int, Int), essentially
type Point = V2 Int

data Edge = E Point Point
  deriving (Show, Eq, Ord)

-- | Make an edge.  Normalizes so we can compare for uniqueness.
mkEdge :: Point -> Point -> Edge
mkEdge x y
  | x <= y    = E x y
  | otherwise = E y x

-- | Parse a stream of `RegTok`.  We have a State of the "current point".
type Parser = P.Parsec [RegTok] Point
```

We either have a "normal step", or a "branching step".  The entire way, we
accumulate a set of all edges.

```haskell
tok :: RegTok -> Parser ()
tok t = P.try $ guard . (== t) =<< P.anyToken

-- | `anySteps` is many normal steps or branch steps.  Each of these gives an
-- edge, so we union all of their edges together.
anySteps :: Parser (Set Edge)
anySteps = fmap S.unions . P.many $
    P.try normalStep P.<|> branchStep

-- | `normalStep` is a normal step without any branching.  It is an `RTDir`
-- token, followed by `anySteps`.  We add the newly discovered edge to the
-- edges in `anySteps`.
normalStep :: Parser (Set Edge)
normalStep = do
    currPos <- P.getState
    RTDir d <- P.anyToken
    let newPos = currPos + case d of
          DN -> V2   0 (-1)
          DE -> V2   1   0
          DS -> V2   0   1
          DW -> V2 (-1)  0
    P.setState newPos
    S.insert (mkEdge currPos newPos) <$> anySteps

-- | `branchStep` is many `anySteps`, each separated by an `RTOr` token.  It is
-- located between `RTRParen` and `RTLParen`.
branchStep :: Parser (Set Edge)
branchStep = (tok RTRParen `P.between` tok RTLParen) $ do
    initPos <- P.getState
    fmap S.unions . (`P.sepBy` tok RTOr) $ do
      P.setState initPos
      anySteps
```

Our final regexp parser is just `anySteps` seperated by the start and end
tokens:

```haskell
buildEdges :: Parser (Set Edge)
buildEdges = (tok RTStart `P.between` tok RTEnd) anySteps
```

Now that we have successfully parsed the "regexp" into a set of edges, we need
to follow all of the edges into all of the rooms.  We can do this using
recursive descent.

```haskell
neighbs :: Point -> [Point]
neighbs p = (p +) <$> [ V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0 ]


roomDistances :: Set Edge -> [Int]
roomDistances es = go 0 S.empty (V2 0 0)
  where
    go :: Int -> Set Point -> Point -> [Int]
    go n seen p = (n :) $
        concatMap (go (n + 1) (S.insert p seen)) allNeighbs
      where
        allNeighbs = filter ((`S.member` es) . mkEdge p)
                   . filter (`S.notMember` seen)
                   $ neighbs p
```

We have to make sure to keep track of the "already seen" rooms.  On my first
attempt, I forgot to do this!

Anyway, here's Part 1 and Part 2:

```haskell
day20a :: String -> Int
day20a inp = maximum (roomDistances edges)
  where
    Right edges = P.runParser buildEdges (V2 0 0) ""
                    (parseToks inp)

day20b :: String -> Int
day20b inp = length . filter (>= 1000) $ roomDistances edges
  where
    Right edges = P.runParser buildEdges (V2 0 0) ""
                    (parseToks inp)
```
