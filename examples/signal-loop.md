# signal-loop → three-signal branching for per-stage metered loops

`Signal s r = Continue s | Fallback s | Done r` gives three-way branching
in Kleisli space — no new GADT constructor, no `Either` tensor required.

Re-exported from `Circuit.Perf` (or import `Circuit.Perf.Signal` directly).

## the pieces

```haskell
import Circuit.Perf (Signal (..), (<|>), loopAlt)

data Signal s r = Continue s | Fallback s | Done r

(<|>) :: Monad m => Kleisli m s (Signal s r)
      -> Kleisli m s (Signal s r)
      -> Kleisli m s (Signal s r)
-- tries first; if Fallback, tries second. Continue/Done pass through.

loopAlt :: Monad m => Kleisli m s (Signal s r) -> Kleisli m s r
-- feeds Continue back, stops on Done. Expects Fallback consumed by <|>.
```

Because it works for any `Monad m`, it applies to pure functions
(`Kleisli Identity`, i.e. `(->)`) as well as effectful pipelines
(`Kleisli IO`).

## word-counting loop

```haskell
processOneLine <|> done
```

- `processOneLine`: reads a line, counts words, signals `Continue`.
  On EOF, signals `Fallback` → `<|>` chains to `done`.
- `done`: extracts the accumulated counts, signals `Done` → `loopAlt` stops.

Each sub-stage (`hIsEOF`, `hGetLine`, `getWords`) can be metered by
threading a measurement map through the state.  Per-stage metering works
because nothing wraps the output in `(Nanos, ...)` — the measurement map
rides alongside as state.

## vs process pipeline

| approach | branching | metering | tensor |
|----------|-----------|----------|--------|
| `wordCountLineByLineMetered` | `bool` on hIsEOF | `meterNamed` per stage | Kleisli `(,)` (explicit state) |
| `wordCountSignal` | `Continue`/`Fallback`/`Done` + `<|>` | `meterNamed` per stage | Kleisli `(,)` (explicit state) |
| `Knot` with `Either` | `Left`/`Right` in step function | blocked (meterK breaks Either shape) | `Either` (trace iteration) |

The `Signal` approach separates "keep looping" from "try alternative" from "done"
— three distinct control signals, not two conflated into `Left`. The `<|>`
combinator chains stages without nesting conditionals.

## relation to circuits

`Signal`/`<|>`/`loopAlt` is a **bridge** from `Either`-trace iteration to
`(,)`-trace state threading.  `Either` gives iteration for free via `Knot`,
but breaks when outputs are wrapped (e.g. by `meterK`).  `Signal` rebuilds
the same control flow by hand on `(,)`, keeping the state type intact.

It lives in `Kleisli m`, not `Circuit`.  For Circuit-native looping with
metering, the path is: use `(,)` tensor + `ambient` to thread the
measurement map + explicit `hIsEOF`/`hGetLine` in the Kleisli.

## verified

- [x] per-stage metering with three-signal branching
- [x] 2921 unique words (matches all other pipelines)
- [ ] Circuit-native version with ambient + (,) tensor
