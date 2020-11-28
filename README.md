Advent of Code 2018
===================

*[2016][]* / *[2017][]* / *2018* / *[2019][]*

[2016]: https://github.com/mstksg/advent-of-code-2016
[2017]: https://github.com/mstksg/advent-of-code-2017
[2019]: https://github.com/mstksg/advent-of-code-2019

It's the most wonderful time of the year!

My [Advent of Code 2018][aoc2018] Haskell solutions here, along with an automated
fetching, testing, running environment (powered by the
*[advent-of-code-api][]* library).  The interactive development environment and
runner/bench marker/viewer/tester has been pulled out [here][dev], so this is
implemented as "fork" of it with my own solutions and reflections.

Check out reflections and commentary at the [package haddocks][haddock]!
(individual links down below)

[aoc2018]: https://adventofcode.com/2018
[haddock]: https://mstksg.github.io/advent-of-code-2018/
[advent-of-code-api]: https://hackage.haskell.org/package/advent-of-code-api
[dev]: https://github.com/mstksg/advent-of-code-dev

[Reflections and Benchmarks][reflections]
-----------------------------------------

*   **[Day 1 Reflections][d01r]** *[code][d01g]* / *[rendered][d01h]* / *[benchmarks][d01b]*
*   **[Day 2 Reflections][d02r]** *[code][d02g]* / *[rendered][d02h]* / *[benchmarks][d02b]*
*   **[Day 3 Reflections][d03r]** *[code][d03g]* / *[rendered][d03h]* / *[benchmarks][d03b]*
*   **[Day 4 Reflections][d04r]** *[code][d04g]* / *[rendered][d04h]* / *[benchmarks][d04b]*
*   **[Day 5 Reflections][d05r]** *[code][d05g]* / *[rendered][d05h]* / *[benchmarks][d05b]*
*   **[Day 6 Reflections][d06r]** *[code][d06g]* / *[rendered][d06h]* / *[benchmarks][d06b]*
*   **[Day 7 Reflections][d07r]** *[code][d07g]* / *[rendered][d07h]* / *[benchmarks][d07b]*
*   **[Day 8 Reflections][d08r]** *[code][d08g]* / *[rendered][d08h]* / *[benchmarks][d08b]*
*   **[Day 9 Reflections][d09r]** *[code][d09g]* / *[rendered][d09h]* / *[benchmarks][d09b]*
*   **[Day 10 Reflections][d10r]** *[code][d10g]* / *[rendered][d10h]* / *[benchmarks][d10b]*
*   **[Day 11 Reflections][d11r]** *[code][d11g]* / *[rendered][d11h]* / *[benchmarks][d11b]*
*   **[Day 12 Reflections][d12r]** *[code][d12g]* / *[rendered][d12h]* / *[benchmarks][d12b]*
*   **[Day 13 Reflections][d13r]** *[code][d13g]* / *[rendered][d13h]* / *[benchmarks][d13b]*
*   **[Day 14 Reflections][d14r]** *[code][d14g]* / *[rendered][d14h]*
*   **Day 15 Reflections** *[code][d15g]* / *[rendered][d15h]*
*   **[Day 16 Reflections][d16r]** *[code][d16g]* / *[rendered][d16h]* / *[benchmarks][d16b]*
*   **Day 17 Reflections** *[code][d17g]* / *[rendered][d17h]*
*   **Day 18 Reflections** *[code][d18g]* / *[rendered][d18h]*
*   **Day 19 Reflections** *[code][d19g]* / *[rendered][d19h]*
*   **[Day 20 Reflections][d20r]** *[code][d20g]* / *[rendered][d20h]* / *[benchmarks][d20b]*
*   **Day 21 Reflections** *[code][d21g]* / *[rendered][d21h]*
*   **Day 22 Reflections** *[code][d22g]* / *[rendered][d22h]*
*   **Day 23 Reflections** *[code][d23g]* / *[rendered][d23h]*
*   **Day 24 Reflections** *[code][d24g]* / *[rendered][d24h]*

"Rendered" links go to haddock source renders for code, with reflections in the
documentation.  Haddock source renders have hyperlinked identifiers,
so you can follow any unrecognized identifiers to see where I have defined them
in the library.

[reflections]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md

### `:~>` type

If you're looking at my actual github solutions, you'll notice thattThis year
I'm implementing my solutions in terms of a `:~>` record type:

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

You might also notice the function `dyno_`, used like `dyno_ "limit" 10000`.  This
is how I implement parameters in problems that vary between test data and
actual input.  For example, Day 6 involved finding points that had a total
distance of less than 10000, but for the test input, we found the points that
had a total distance of less than 32.  So, I have a system that lets me write
`dyno_ "limit" 10000` in my code instead of hard-coding in `10000`.  This
`10000` would be replaced by `32` when running with test data (which is parsed
from [this file][7btest])

[7btest]: https://github.com/mstksg/advent-of-code-2018/blob/master/test-data/06b.txt

Interactive
-----------

The *[AOC2018.Run.Interactive][interactive]* module has code (powered by
*[advent-of-code-api][]*) for testing your solutions and submitting within
GHCI, so you don't have to re-compile. If you edit your solution programs, they
are automatically updated when you hit `:r` in ghci.

[interactive]: https://mstksg.github.io/advent-of-code-2018/AOC2018-Run-Interactive.html

```haskell
ghci> execSolution_   $ solSpec 'day02a  -- get answer for challenge based on solution
ghci> testSolution_   $ solSpec 'day02a  -- run solution against test suite
ghci> viewPrompt_     $ solSpec 'day02a  -- view the prompt for a part
ghci> waitForPrompt_  $ solSpec 'day02a  -- count down to the prompt for a part
ghci> submitSolution_ $ solSpec 'day02a  -- submit a solution
```

These are loaded with session key stored in the configuration file (see next
section).

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

Usage: aoc2018 [-c|--config PATH] COMMAND
  Run challenges from Advent of Code 2018. Available days: 1, 2, 3 (..)

Available options:
  -c,--config PATH         Path to configuration file (default: aoc-conf.yaml)
  -h,--help                Show this help text

Available commands:
  run                      Run, test, and benchmark challenges
  view                     View a prompt for a given challenge
  submit                   Test and submit answers for challenges
  test                     Alias for run --test
  bench                    Alias for run --bench
  countdown                Alias for view --countdown

$ aoc2018 run 3 b
>> Day 03b
>> [✓] 243
```

You can supply input via stdin with `--stdin`:

```
$ aoc2018 run 1 --stdin
>> Day 01a
+1
+2
+1
-3
<Ctrl+D>
[?] 1
>> Day 01b
[?] 1
```

Benchmarking is implemented using *criterion*

```
$ aoc2018 bench 2
>> Day 02a
benchmarking...
time                 1.317 ms   (1.271 ms .. 1.392 ms)
                     0.982 R²   (0.966 R² .. 0.999 R²)
mean                 1.324 ms   (1.298 ms .. 1.373 ms)
std dev              115.5 μs   (77.34 μs .. 189.0 μs)
variance introduced by outliers: 65% (severely inflated)

>> Day 02b
benchmarking...
time                 69.61 ms   (68.29 ms .. 72.09 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 69.08 ms   (68.47 ms .. 69.99 ms)
std dev              1.327 ms   (840.8 μs .. 1.835 ms)
```

Test suites run the example problems given in the puzzle description, and
outputs are colorized in ANSI terminals.

```
$ aoc2018 test 1
>> Day 01a
[✓] (3)
[✓] (3)
[✓] (0)
[✓] (-6)
[✓] Passed 4 out of 4 test(s)
[✓] 416
>> Day 01b
[✓] (2)
[✓] (0)
[✓] (10)
[✓] (5)
[✓] (14)
[✓] Passed 5 out of 5 test(s)
[✓] 56752
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

These are stored in `data/ans/XXpart.txt`.  That is, the target output for Day 7
(Part 2, `b`) will be expected at `data/ans/07b.txt`.  You can also manually
edit these files.

You can view prompts: (use `--countdown` to count down until a prompt is
released, and display immediately)

```
$ aoc2018 view 3 b
>> Day 03b
--- Part Two ---
----------------

Amidst the chaos, you notice that exactly one claim doesn't overlap by
even a single square inch of fabric with any other claim. If you can
somehow draw attention to it, maybe the Elves will be able to make
Santa's suit after all!

For example, in the claims above, only claim `3` is intact after all
claims are made.

*What is the ID of the only claim that doesn't overlap?*
```

You can also submit answers:

```
$ aoc2018 submit 1 a
```

Submissions will automatically run the test suite.  If any tests fail, you will
be asked to confirm submission or else abort.  The submit command will output
the result of your submission: The message from the AoC website, and whether or
not your answer was correct (or invalid or ignored).  Answers that are
confirmed correct will be locked in and saved for future testing against, in
case you change your solution.

All networking features are powered by *[advent-of-code-api][]*.

[d01g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day01.hs
[d01h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day01.html
[d01r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-1
[d01b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-1-benchmarks

[d02g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day02.hs
[d02h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day02.html
[d02r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-2
[d02b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-2-benchmarks

[d03g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day03.hs
[d03h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day03.html
[d03r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-3
[d03b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-3-benchmarks

[d04g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day04.hs
[d04h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day04.html
[d04r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-4
[d04b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-4-benchmarks

[d05g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day05.hs
[d05h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day05.html
[d05r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-5
[d05b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-5-benchmarks

[d06g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day06.hs
[d06h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day06.html
[d06r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-6
[d06b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-6-benchmarks

[d07g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day07.hs
[d07h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day07.html
[d07r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-7
[d07b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-7-benchmarks

[d08g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day08.hs
[d08h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day08.html
[d08r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-8
[d08b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-8-benchmarks

[d09g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day09.hs
[d09h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day09.html
[d09r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-9
[d09b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-9-benchmarks

[d10g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day10.hs
[d10h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day10.html
[d10r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-10
[d10b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-10-benchmarks

[d11g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day11.hs
[d11h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day11.html
[d11r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-11
[d11b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-11-benchmarks

[d12g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day12.hs
[d12h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day12.html
[d12r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-12
[d12b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-12-benchmarks

[d13g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day13.hs
[d13h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day13.html
[d13r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-13
[d13b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-13-benchmarks

[d14g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day14.hs
[d14h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day14.html
[d14r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-14

[d15g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day15.hs
[d15h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day15.html

[d16g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day16.hs
[d16h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day16.html
[d16r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-16
[d16b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-16-benchmarks

[d17g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day17.hs
[d17h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day17.html

[d18g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day18.hs
[d18h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day18.html

[d19g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day19.hs
[d19h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day19.html

[d20g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day20.hs
[d20h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day20.html
[d20r]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-20
[d20b]: https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-20-benchmarks

[d21g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day21.hs
[d21h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day21.html

[d22g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day22.hs
[d22h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day22.html

[d23g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day23.hs
[d23h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day23.html

[d24g]: https://github.com/mstksg/advent-of-code-2018/blob/master/src/AOC/Challenge/Day24.hs
[d24h]: https://mstksg.github.io/advent-of-code-2018/src/AOC.Challenge.Day24.html
