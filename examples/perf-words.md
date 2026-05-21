# perf-words ⟜ raw speed of word-counting pipelines

First pass at metering the word-counting pipelines with circuits-meter.
Start with the simplest thing: wrap `wordCountAllAtOnceFile` in a Kleisli,
meter it, record the number.

## all-at-once

Wrap the IO action in a Kleisli, meter with `timeM`:

```haskell
import Words (wordCountAllAtOnceFile, wordCountLineByLineFile)
import Circuit.Perf (meterK, timesK)
import Circuit.Perf.Time (timeM)
import Control.Arrow (Kleisli (..), runKleisli)

-- single run
(t, ()) <- runKleisli (meterK timeM (Kleisli (\_ -> wordCountAllAtOnceFile))) ()
putStrLn $ "all-at-once: " ++ show t ++ " ns"
```

| run | all-at-once (ns) | note |
|-----|-----------------|------|
| 1   | 37,496,042      | repl (interpreted) |
|     |                 |                    |
|     |                 |                    |

## line-by-line

Same pattern for the line-by-line pipeline:

```haskell
(t, ()) <- runKleisli (meterK timeM (Kleisli (\_ -> wordCountLineByLineFile))) ()
putStrLn $ "line-by-line: " ++ show t ++ " ns"
```

| run | line-by-line (ns) | note |
|-----|-------------------|------|
|     |                   |      |

## compiled (-O2)

Repl numbers are 10-100x slower than compiled. To get real numbers,
add an exe or use `once`/`timesC` from a compiled context.

## next

- Lift into `Circuit (Kleisli IO) (,) () ()` via `Lift`
- Meter with bracket syntax: `timeM ◅ pipeline ▻ timeM`
- Per-stage metering: meter `readFile` separately from `countWords` from `putStr`
