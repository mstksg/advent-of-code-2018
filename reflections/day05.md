**My write-up for this is actually [on my blog, here][d05b]!**  It involves my
group theory/free group/group homomorphism based solution.  That's my main
reflection, but I also had a method that I wrote *before*, that I would still
like to preserve.

So, preserved here was my original solution involving `funkcyCons` and `foldr`:

One of the first higher-order functions you learn about in Haskill is `foldr`,
which is like a "skeleton transformation" of a list.

That's because in Haskell, a (linked) list is one of two constructors: nil
(`[]`) or cons (`:`).  The list `[1,2,3]` is really `1:(2:(3:[]))`.

`foldr f z` is a function that takes a list replaces all `:`s with `f`, and
`[]`s with `z`s:

```haskell
          [1,2,3] = 1  :  (2  :  (3  :  []))
foldr f z [1,2,3] = 1 `f` (2 `f` (3 `f` z ))
```

This leads to one of the most famous identities in Haskell: `foldr (:) [] xs =
xs`.  That's because if we go in and replace all `(:)`s with `(:)`, and replace
all `[]`s with `[]`... we get back the original list!

But something we can also do is give `foldr` a "custom cons".  A custom cons
that will go in place of the normal cons.

This problem is well-suited for such a custom cons: instead of normal `(:)`,
we'll write a custom cons that respects the rules of reaction: we can't have
two "anti-letters" next to each other:

```haskell
anti :: Char -> Char -> Bool
anti x y = toLower x == toLower y && x /= y

funkyCons :: Char -> String -> String
x `funkyCons` (y:xs)
    | anti x y  = xs
    | otherwise = x:y:xs
x `funkyCons` []     = [x]
```

So, `foldr funkyCons []` will go through a list and replace all `(:)` (cons)
with `funkyCons`, which will "bubble up" the reaction.

So, that's just the entire part 1!

```haskell
day05a :: String -> Int
day05a = length . foldr funkyCons []
```

For part 2 we can just find the minimum length after trying out every
character.

```haskell
day05b :: String -> Int
day05b xs = minimum [ length $ foldr funkyCons [] (remove c xs)
                    | c <- ['a' .. 'z']
                    ]
  where
    remove c = filter ((/= c) . toLower)
```

(Note that in the actual input, there is a trailing newline, so in practice we
have to strip it from the input.)
