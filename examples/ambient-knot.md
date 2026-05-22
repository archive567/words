# ambient + (,) + Knot

Threading a measurement map through a `Knot` with the cartesian tensor.

## meteredAmbient

`meteredAmbient` wraps a `Kleisli` stage with a meter and threads the
reading into an ambient state wire:

```haskell
meteredAmbient :: (s -> t -> s) -> Meter s t -> Kleisli IO a b
             -> Circuit (Kleisli IO) (,) (s, a) (s, b)
```

The state `s` rides `(,)` alongside the payload.  Each stage reads `s`,
runs the metered computation, and returns the updated state.  Compose
stages with `Compose` (or `⊙` / `↣` from `Circuit.Perf`):

```haskell
import Circuit (Circuit(..), reify, ambient)
import Circuit.Perf (meteredAmbient, timeM)
import Circuit.Perf.Time (timeM)
import Control.Arrow (Kleisli(..), runKleisli)
import qualified Data.Map.Strict as Map

let accum m t = Map.insertWith (++) "stage" [t] m
let stage1 = meteredAmbient accum timeM (Kleisli getWords)
let stage2 = meteredAmbient accum timeM (Kleisli countWords)

let pipeline = stage2 `Compose` stage1
    -- (Map, String) -> (Map, Map String Int)
```

## ambient through Knot

`ambient` slides the state wire past the feedback channel of a `Knot`:

```haskell
ambient :: Circuit arr t a b -> Circuit arr t (t s a) (t s b)
```

For `(,)`: `ambient` turns a `Circuit (Kleisli IO) (,) a b` into a circuit
on `(s, a) -> (s, b)`.  For `Knot`, the braid swaps the state past the
feedback wire so the two don't collide.

## pure example: metered lazy fibs

A `Knot` with `(,)` ties a lazy knot — cyclic sharing, not strict
iteration.  The metered body generates Fibonacci numbers while
accumulating timings in a map:

```haskell
{-# LANGUAGE BlockArguments #-}

import Circuit
import Circuit.Perf (meteredAmbient, timeM, Nanos)
import Control.Arrow (Kleisli(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Accumulate nanosecond readings into a map
accum :: Map String [Nanos] -> Nanos -> Map String [Nanos]
accum m t = Map.insertWith (++) "fib" [t] m

-- One step of the fibonacci generator, metered
fibStep :: Circuit (->) (,) (Map String [Nanos], ()) (Map String [Nanos], [Int])
fibStep = meteredAccum accum timeM (Kleisli $ \() -> pure fibs)
  where
    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

-- Thread the map through the knot
fibMetered :: Circuit (->) (,) () [Int]
fibMetered = ambient fibStep
  -- Note: this is a sketch; the actual wiring depends on how you want
  -- the map to flow through the lazy knot.
```

The key point: `meteredAmbient` keeps the measurement map on the `(,)`
tensor, so `ambient` can braid it past `Knot` without collision.

## IO: the iteration problem

`Knot` with `(,)` on `Kleisli IO` uses `mfix` — a lazy knot.  For strict
iteration (file reading, line-by-line loops), `mfix` diverges because the
body forces the feedback value before producing output.

For strict IO iteration, use `Iter` + `loopIter` instead:

```haskell
import Circuit (Iter(..), loopIter)

let step = Kleisli $ \(h, acc) -> do
      eof <- hIsEOF h
      if eof then pure (Exit acc) else do
        line <- hGetLine h
        pure (Loop (h, foldl' ... acc (getWords line)))

runKleisli (loopIter step) (handle, Map.empty)
```

Per-stage metering inside an `Iter` loop uses `meteredAmbient` on each
sub-stage, keeping the measurement map in the `(,)` state wire.

## the gap, restated

| Tensor | Loop mechanism | Works with `meterK`? | Strict IO? |
|--------|---------------|----------------------|------------|
| `Either` | `trace` iterates via `Knot` | No — `meterK` wraps `(,)` | Yes |
| `(,)` | `mfix` lazy knot via `Knot` | Yes — same tensor | No |
| `(,)` | `Iter` + `loopIter` explicit | Yes — same tensor | Yes |

`ambient + (,) + Knot` is the pattern for **lazy feedback** with metering.
For **strict iteration** with metering, the pattern is `ambient + (,) + Iter`.
