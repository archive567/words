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
import System.IO (readFile)

-- read once, force to NF so timing is clean
contents <- evaluate . force =<< readFile "other/alice.md"

-- n runs, returns ([Nanos], result)
(ts, counts) <- ticks 10 countWords contents

putStrLn $ "runs:  " ++ show (length ts)
putStrLn $ "min:   " ++ show (minimum ts) ++ " ns"
putStrLn $ "p50:   " ++ show (sort ts !! (length ts `div` 2)) ++ " ns"
```

| n | min (ns) | p50 (ns) | note |
|---|----------|----------|------|
|   |          |          | repl |
|   |          |          |      |

## full pipeline (timesK)

Time the whole IO action — file read + count + print:

```haskell
import Words (wordCountAllAtOnceFile, wordCountLineByLineFile)
import Circuit.Perf (timesK)
import Circuit.Perf.Time (timeM)
import Control.Arrow (Kleisli (..), runKleisli)

-- all-at-once
(ts, ()) <- runKleisli (timesK 5 timeM (Kleisli (\_ -> wordCountAllAtOnceFile))) ()
putStrLn $ "all-at-once p50: " ++ show (sort ts !! (length ts `div` 2)) ++ " ns"

-- line-by-line
(ts2, ()) <- runKleisli (timesK 5 timeM (Kleisli (\_ -> wordCountLineByLineFile))) ()
putStrLn $ "line-by-line p50: " ++ show (sort ts2 !! (length ts2 `div` 2)) ++ " ns"
```

| pipeline | n | p50 (ns) | note |
|----------|---|----------|------|
| all-at-once | | | repl |
| line-by-line | | | repl |

## compiled (-O2)

Repl numbers are 10-100x slower. For real numbers, compile with `-O2`:

```bash
cabal exec ghc -- -O2 -o /tmp/perf-words examples/perf-words.hs && /tmp/perf-words
```

| pipeline | min (ns) | p50 (ns) | note |
|----------|----------|----------|------|
| all-at-once | | | -O2 |
| line-by-line | | | -O2 |

## next

- Lift into `Circuit (Kleisli IO) (,) () ()` — bracket syntax
- Per-stage metering: `timeM ◅ readFile ▻ timeM` then `timeM ◅ countWords ▻ timeM`
- `once` for single-shot measurement
