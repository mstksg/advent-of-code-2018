Advent of Code 2018
===================

It's the most wonderful time of the year!

My [Advent of Code 2018][aoc2018] Haskell solutions here, along with an automated
fetching, testing, running environment.

Check out reflections and commentary at the [package haddocks][haddock]!
(individual links down below)

[aoc2018]: https://adventofcode.com/2018
[haddock]: https://mstksg.github.io/advent-of-code-2018/

Challenges and Reflections
--------------------------

Links go to haddock source renders for code, with reflections in the
documentation.  Haddock source renders have hyperlinked identifiers,
so you can follow any unrecognized identifiers to see where I have defined them
in the library.  Benchmark times for each part are listed after each link.

*   **Day 1**: [Rendered][day01r] / [Github][day01g] *( 2.9 ms / 140 ms )*
*   **Day 2**: [Rendered][day02r] / [Github][day02g] *( 1.2 ms / 50. ms )*

[day01r]: https://mstksg.github.io/advent-of-code-2018/src/AOC2018.Challenge.Day01.html
[day01g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC2018/Challenge/Day01.hs
[day02r]: https://mstksg.github.io/advent-of-code-2018/src/AOC2018.Challenge.Day02.html
[day02g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC2018/Challenge/Day02.hs

### `:~>` type

This year I'm implementing my solutions in terms of a `:~>` record type:

```haskell
data a :~> b = MkSol
    { sParse :: String -> Maybe a    -- ^ parse input into an `a`
    , sSolve :: a      -> Maybe b    -- ^ solve an `a` input to a `b` solution
    , sShow  :: b      -> String     -- ^ print out the `b` solution for submission
    }
```

An `a :~> b` is a solution to a challenge expecting input of type `a` and
producing answers of type `b`.  It also packs in functions to parse a `String`
into an `a`, and functions to show a `b` as a `String` to submit as an answer.

This helps me mentally separate out parsing, solving, and showing, allowing for
some cleaner code and an easier time planning my solution.

Such a challenge can be "run" on string inputs by feeding the string into
`sParse`, then `sSolve`, then `sShow`:

```haskell
-- | Run a ':~>' on some input, retuning 'Maybe'
runSolution :: Challenge -> String -> Maybe String
runSolution MkSol{..} s = do
    x <- sParse s
    y <- sSolve x
    pure $ sShow y
```

In the actual library, I have `runSolution` return an `Either` so I can debug
which stage the error happened in.

Executable
----------

Comes with test examples given in problems.

You can install using `stack`:

```bash
$ git clone https://github.com/mstksg/advent-of-code-2018
$ cd advent-of-code-2018
$ stack setup
$ stack install
```

The executable `aoc2018` includes a testing and benchmark suite, as well as a
way to view prompts within the command line:

```
$ aoc2018 --help
aoc2018 - Advent of Code 2018 challenge runner

Usage: aoc2018 DAY [PART] [-p|--prompt] [-n|--no-run] [-t|--tests] [-b|--bench]
  Run challenges from Advent of Code 2018

Available options:
  DAY                      Day of challenge (1 - 25), or "all"
  PART                     Challenge part (a, b, c, etc.)
  -p,--prompt              Show problem prompt before running
  -n,--no-run              Do not run solution on input
  -t,--tests               Run sample tests
  -b,--bench               Run benchmarks
  -h,--help                Show this help text

$ aoc2018 5 b
>> Day 05b
>> [✓] 27720699
```

Benchmarking is implemented using *criterion*

```
$ aoc2018 2 --bench
>> Day 02a
benchmarking...
time                 729.1 μs   (695.0 μs .. 784.2 μs)
                     0.967 R²   (0.926 R² .. 0.995 R²)
mean                 740.4 μs   (711.9 μs .. 783.6 μs)
std dev              116.8 μs   (70.44 μs .. 172.8 μs)
variance introduced by outliers: 89% (severely inflated)

>> Day 02b
benchmarking...
time                 782.4 μs   (761.3 μs .. 812.9 μs)
                     0.983 R²   (0.966 R² .. 0.998 R²)
mean                 786.7 μs   (764.1 μs .. 849.4 μs)
std dev              110.8 μs   (42.44 μs .. 228.5 μs)
variance introduced by outliers: 86% (severely inflated)
```

Test suites run the example problems given in the puzzle description, and
outputs are colorized in ANSI terminals.

```
$ aoc2018 1 --tests
[9] [!35732] $ aoc2018 1 --tests
>> Day 01a
[✓] (3)
[✓] (4)
[✓] (0)
[✓] (9)
[✓] Passed 4 out of 4 test(s)
[✓] 1097
>> Day 01b
[✓] (6)
[✓] (0)
[✓] (4)
[✓] (12)
[✓] (4)
[✓] Passed 5 out of 5 test(s)
[✓] 1188
```

This should only work if you're running `aoc2018` in the project directory.

**To run on actual inputs**, the executable expects inputs to be found in the
folder `data/XX.txt` in the directory you are running in.  That is, the input
for Day 7 will be expected at `data/07.txt`.

*aoc2018 will download missing input files*, but requires a session token.
This can be provided in `aoc-conf.yaml`:

```yaml
session:  [[ session token goes here ]]
```

Session keys are also required to download "Part 2" prompts for each challenge.

You can "lock in" your current answers (telling the executable that those are
the correct answers) by passing in `--lock`.  This will lock in any final
puzzle solutions encountered as the verified official answers.  Later, if you
edit or modify your solutions, they will be checked on the locked-in answers.

These are store in `data/ans/XXpart.txt`.  That is, the target output for Day 7
(Part 2, `b`) will be expected at `data/ans/07b.txt`.  You can also manually
edit these files.
