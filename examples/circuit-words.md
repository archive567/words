# circuit-words ⟜ process pipeline as Circuit stages

The left-to-right pipeline from Words.hs, decomposed into named Circuit stages.
Each `(.>)` becomes a `Compose`. Each pure function becomes a `Lift`.

## pure Circuit composition

The all-at-once counting pipeline as three `Lift` stages:

```haskell
import Words (countWords, formatTop)
import Circuit.Circuit (Circuit(..), reify)
import Data.Map.Strict (Map)

-- each stage is a named Circuit
let countStage = Lift countWords
let formatStage = Lift (formatTop 5)

-- compose (formatStage runs after countStage)
let pipeline = formatStage `Compose` countStage

-- reify pins the tensor so GHC can resolve the Trace instance
let reify' = reify :: Circuit (->) (,) String String -> String -> String

-- >>> putStr $ reify' pipeline "the cat and the cat"
-- cat: 2
-- the: 2
-- and: 1
```

This is the function-space pipeline (`(.>)`) mapped to Circuit space (`Compose`).
Same structure, same output, same intermediate types. But `reify` can walk the
tree, and `Circuit.Perf` can instrument each stage.

## metering with meterK

For timing, the stages become `Kleisli IO` arrows. `meterK` wraps each one:

```haskell
import Circuit.Perf (meterK)
import Circuit.Perf.Time (timeM)
import Control.Arrow (Kleisli(..), runKleisli)

-- wrap each stage as a Kleisli, then meter
let countMetered = meterK timeM (Kleisli (pure . countWords))
let formatMetered = meterK timeM (Kleisli (pure . formatTop 5))

-- run both, collecting timings
contents <- readFile "other/alice.md"
(tCount, counts) <- runKleisli countMetered contents
(tFmt, result) <- runKleisli formatMetered counts
putStrLn $ "count:  " ++ show tCount ++ " ns"
putStrLn $ "format: " ++ show tFmt ++ " ns"
```

## bracket syntax

With stages as `Circuit (Kleisli IO) (,)`, the `◅`/`▻` brackets apply:

```haskell
import Circuit.Circuit (Circuit(..), reify)
import Circuit.Perf (Nanos, Meter(..), (◅), (▻), postC)
import Circuit.Perf.Time (timeM)
import Control.Arrow (Kleisli(..), runKleisli)

let countStage = Lift (Kleisli (pure . countWords))
let formatStage = Lift (Kleisli (pure . formatTop 5))

-- meter the composed pipeline, discarding measurement
let timed = timeM ◅ (formatStage `Compose` countStage) ▻ timeM

-- meter and keep the measurement
let measured = postC (post timeM) `Compose` (timeM ◅ (formatStage `Compose` countStage))

let reify' = reify :: Circuit (Kleisli IO) (,) String (Nanos, String) -> Kleisli IO String (Nanos, String)
(t, result) <- runKleisli (reify' measured) "the cat and the cat"
-- result: "cat: 2\nthe: 2\nand: 1"
-- t: ~4000 ns
```

## process ↔ circuit map

| process style     | Circuit space               |
|-------------------|-----------------------------|
| `f .> g`          | `g `Compose` f`             |
| `x |> f`          | `reify (Lift f) x`          |
| `f >-> g`         | `Knot` / `trace`            |
| `pure f`          | `Lift (Kleisli (pure . f))` |
| `timeM ◅ ... ▻`   | `meterC_ timeM`             |

## next

- Line-by-line loop as `Knot` with `Either` tensor
- Full file bracket: `openFile ◅ pipeline ▻ closeFile`
- Per-stage timing on the line-by-line pipeline
