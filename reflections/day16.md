Today was fun because I got to re-use some techniques I discussed in a blog
post I've written in the past: [Send More Money: List and
StateT][send-more-money].  I talk about using `StateT` over `[]` to do
implement prolog-inspired constraint satisfaction searches while taking
advantage of laziness.

[send-more-money]: https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet.html

First of all, our types.  I'll be using the *[vector-sized][]* library with
*[finite-typelits][]* to help us do safe indexing.  A `Vector n a` is a vector
of `n` `a`s, and a `Finite n` is a legal index into such a vector.  For
example, a `Vector 4 Int` is a vector of 4 `Int`s, and `Finite 4` is 0, 1, 2,
or 3.

[vector-sized]: https://hackage.haskell.org/package/vector-sized
[finite-typelits]: https://hackage.haskell.org/package/finite-typelits

```haskell
import           Data.Vector.Sized (Vector)
import           Data.Finite       (Finite)

type Reg = Vector 4 Int

data Instr a = I { _iOp  :: a
                 , _iInA :: Finite 4
                 , _iInB :: Finite 4
                 , _iOut :: Finite 4
                 }
  deriving (Show, Functor)

data Trial = T { _tBefore :: Reg
               , _tInstr  :: Instr (Finite 16)
               , _tAfter  :: Reg
               }
  deriving Show

data OpCode = OAddR | OAddI
            | OMulR | OMulI
            | OBanR | OBanI
            | OBorR | OBorI
            | OSetR | OSetI
            | OGtIR | OGtRI | OGtRR
            | OEqIR | OEqRI | OEqRR
  deriving (Show, Eq, Ord, Enum, Bounded)
```

We can leave `Instr` parameterized over the opcode type so that we can use it
with `Finite 16` initially, and `OpCode` later.

We do need to implement the functionality of each op, which we can do by
pattern matching on an `OpCode`.  We use some lens functionality to simplify
some of the editing of indices, but we could also just manually modify indices.

```haskell
runOp :: Instr OpCode -> Reg -> Reg
runOp I{..} = case _iOp of
    OAddR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  +  r ^. V.ix _iInB
    OAddI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  +  fromIntegral _iInB
    OMulR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  *  r ^. V.ix _iInB
    OMulI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA  *  fromIntegral _iInB
    OBanR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .&. r ^. V.ix _iInB
    OBanI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .&. fromIntegral _iInB
    OBorR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .|. r ^. V.ix _iInB
    OBorI -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA .|. fromIntegral _iInB
    OSetR -> \r -> r & V.ix _iOut .~ r ^. V.ix _iInA
    OSetI -> \r -> r & V.ix _iOut .~                     fromIntegral _iInA
    OGtIR -> \r -> r & V.ix _iOut . enum .~ (fromIntegral _iInA  > r ^. V.ix _iInB   )
    OGtRI -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA     > fromIntegral _iInB)
    OGtRR -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA     > r ^. V.ix _iInB   )
    OEqIR -> \r -> r & V.ix _iOut . enum .~ (fromIntegral _iInA == r ^. V.ix _iInB   )
    OEqRI -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA    == fromIntegral _iInB)
    OEqRR -> \r -> r & V.ix _iOut . enum .~ (r ^. V.ix _iInA    == r ^. V.ix _iInB   )
```

Now, from a `Trial`, we can get a set of `OpCode`s that are plausible
candidates if the output matches the expected output for a given `OpCode`, for
the given input.

```haskell
plausible :: Trial -> Set OpCode
plausible T{..} = S.fromList (filter tryTrial [OAddR ..])
  where
    tryTrial :: OpCode -> Bool
    tryTrial o = runOp (_tInstr { _iOp = o }) _tBefore == _tAfter
```

Part 1 is, then, just counting the trials with three or more plausible
candidates:

```haskell
day16a :: [Trial] -> Int
day16a = length . filter ((>= 3) . S.size . plausible)
```

Part 2 is where we can implement our constraint satisfaction search.  Following
[this blog post][send-more-money], we can write a search using `StateT (Set
OpCode) []`.  Our state will be the `OpCode`s that we have already used.  We
fill up a vector step-by-step, by picking only `OpCode`s that have not been
used yet:

```haskell
fillIn :: Set OpCode -> StateT (Set OpCode) [] OpCode
fillIn candidates = do
    unseen <- gets (candidates `S.difference`)  -- filter only unseen candidates
    pick   <- lift $ toList unseen              -- branch on all unseen candidates
    modify $ S.insert pick                      -- in this branch, 'pick' is seen
    pure pick                                   -- return our pick for the branch
```

Now, if we have a map of `Finite 16` (op code numbers) to their candidates (a
`Map (Finite 16) (Set OpCode)`), we can populate all legal
configurations.  We'll use `Vector 16 OpCode` to represent our configuration:
`0` will represent the first item, `1` will represent the second, etc.  We can
use `V.generate :: (Finite n -> m a) -> m (Vector n a)`, and run our `fillIn`
action for every `Finite n`.

```haskell
fillVector
    :: Map (Finite 16) (Set OpCode)
    -> StateT (Set OpCode) [] (Vector 16 OpCode)
fillVector candmap = V.generateM $ \i -> do
    Just cands <- pure $ M.lookup i candmap
    fillIn cands

fromClues
    :: Map (Finite 16) (Set OpCode)
    -> Maybe (Vector 16 OpCode)
fromClues m = listToMaybe $ evalStateT (fillVector m) S.empty
```

If this part is confusing, the [blog post][send-more-money] explains how
`StateT` and `[]`, together, give you this short-circuting search behavior!

So our Part 2 is using `fromClues` from all of the candidates (making sure to
do a set intersection if we get more than one clue for an opcode number), and a
`foldl'` over our instruction list:

```haskell
day16b :: [Trial] -> [Instr (Finite 16)] -> Int
day16b ts = V.head . foldl' step (V.replicate 0)
  where
    candmap    = M.fromListWith S.intersection
               $ [ (_iOp (_tInstr t), plausible t)
                 | t <- ts
                 ]
    Just opMap = fromClues candmap
    step r i = runOp i' r
      where
        i' = (opMap `V.index`) <$> i
```
