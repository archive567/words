# perf-words ⟜ raw speed of word-counting pipelines

Measuring word-counting with circuits-meter. Start with `ticks` on the pure
counting function, then meter the full pipeline.

## pure counting (ticks)

Read the file once, then time only the counting logic. This isolates the
algorithm from I/O noise.

```haskell
import Words (countWords)
import Circuit.Perf.Time (ticks)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.List (sort)

-- read once, force to NF so timing is clean
contents <- evaluate . force =<< readFile "other/alice.md"

-- n runs, returns ([Nanos], result)
(ts, counts) <- ticks 10 countWords contents

putStrLn $ "runs:  " ++ show (length ts)
putStrLn $ "min:   " ++ show (minimum ts) ++ " ns"
putStrLn $ "p50:   " ++ show (sort ts !! (length ts `div` 2)) ++ " ns"
```

| n   | min (ns)   | p50 (ns)   | note  |
|-----|------------|------------|-------|
| 10  | 34,617,000 | 35,227,000 | repl  |
| 500 | 19,760,625 | 19,956,041 | -O2   |

The `-O2` p50 is ~20ms to count 2,921 unique words from a 170KB file.
About 6.8 µs per unique word.

## full pipeline (timesK)

Time the whole IO action — file read + count + print:

```haskell
import Words (wordCountAllAtOnceFile, wordCountLineByLineFile)
import Circuit.Perf (timesK)
import Circuit.Perf.Time (timeM)
import Control.Arrow (Kleisli (..), runKleisli)
import Data.List (sort)

-- all-at-once
(ts, ()) <- runKleisli (timesK 5 timeM (Kleisli (\_ -> wordCountAllAtOnceFile))) ()
putStrLn $ "all-at-once p50: " ++ show (sort ts !! (length ts `div` 2)) ++ " ns"
```

| pipeline      | n   | min (ns)   | p50 (ns)   | note  |
|---------------|-----|------------|------------|-------|
| all-at-once   | 5   | 34,587,000 | 35,258,000 | repl  |
| line-by-line  | 5   | 43,673,000 | 44,333,000 | repl  |
| all-at-once   | 100 | 20,339,958 | 20,606,209 | -O2   |
| line-by-line  | 100 | 21,404,125 | 21,713,917 | -O2   |

Line-by-line is only ~1ms slower than all-at-once. The file is ~3,000 lines;
per-line overhead is negligible. The bottleneck is word normalisation and map
insertion, not I/O strategy.

## compiled (-O2)

```bash
cabal run bench-words
```

Runs all three measurements with `-O2` and reports min/p50/mean.

## next

- Lift into `Circuit (Kleisli IO) (,) () ()` — bracket syntax
- Per-stage metering: `timeM ◅ readFile ▻ timeM` then `timeM ◅ countWords ▻ timeM`
