And today features the re-introduction of an Advent of Code staple: the
(circular) tape/zipper!  I used this data structure last year for days 5, 17,
18 and 23, and I consider them near and dear to my heart as Advent of Code data
structures :)

Last year, I wrote my own implementations on the spot, but since then I've come
to appreciate the *[pointed-list][]* library.  A circular tape is a circular
data structure with a "focus" that you can move back and forth in.  This is
the data structure that implements exactly what the challenge talks about!
It's linear-time on "moving the focus", and constant-time on insertions and
deletions.

[pointed-list]: https://hackage.haskell.org/package/pointedlist

The center of everything is the `place` function, which takes a number to place
and a tape to place it in, and returns an updated tape with the "score"
accumulated for that round.

We see that it is mostly a straightforward translation of the problem
statement.  If `x` is a multiple of 23, then we move 7 spaces to the left, and
return the resulting tape with the item deleted.  The score is the deleted item
plus `x`.  Otherwise, we just move 2 spaces to the right and insert `x`, with a
score of 0.

```haskell
place
    :: Int                       -- ^ number to place
    -> PointedList Int           -- ^ tape
    -> (Int, PointedList Int)    -- ^ resulting tape, and scored points
place x l
    | x `mod` 23 == 0
    = let l'       = PL.moveN (-7) l
          toAdd    = _focus l'
      in  (toAdd + x, fromJust (PL.deleteRight l'))
    | otherwise
    = (0, (PL.insertLeft x . PL.moveN 2) l)
```

We wrap it all up with a `run` function, which is a strict fold over a list of
`(currentPlayer, itemToPlace)` pairs, accumulating a `(scorecard, tape)` state
(our scorecard will be a vector where each index is a different player's
score). At each step, we `place`, and use the result to update our scorecard
and tape. The *lens* library offers some nice tool for incrementing a given
index of a vector.

```haskell
run
    :: Int                  -- ^ number of players
    -> Int                  -- ^ Max # of piece
    -> V.Vector Int
run numPlayers maxPiece = fst
                        . foldl' go (V.replicate numPlayers 0, PL.singleton 0)
                        $ zip players toInsert
  where
    go (!scores, !tp) (!player, !x) = (scores & ix player +~ pts, tp')
      where
        (pts, tp') = place x tp
    players  = (`mod` numPlayers) <$> [0 ..]
    toInsert = [1..maxPiece]
```

And that's it!  The answer is just the maximal score in the final score vector:

```haskell
day09a :: Int -> Int -> Int
day09a numPlayers maxPiece = V.maximum (run numPlayers maxPiece)

day09b :: Int -> Int -> Int
day09b numPlayers maxPiece = V.maximum (run numPlayers (maxPiece * 100))
```

From this naive implementation, Part 1 takes 56.ms, and Part 2 takes 4.5s.
