# signal-loop → three-signal branching for per-stage metered loops

`Signal s r = Continue s | Fallback s | Done r` gives three-way branching
in Kleisli space — no new GADT constructor, no `Either` tensor required.

## the pieces

```haskell
data Signal s r = Continue s | Fallback s | Done r

(<|>) :: Kleisli IO s (Signal s r)
      -> Kleisli IO s (Signal s r)
      -> Kleisli IO s (Signal s r)
-- tries first; if Fallback, tries second. Continue/Done pass through.

loopAlt :: Kleisli IO s (Signal s r) -> Kleisli IO s r
-- feeds Continue back, stops on Done. Expects Fallback consumed by <|>.
```

## word-counting loop

```haskell
processOneLine <|> done
```

- `processOneLine`: reads a line, counts words, signals `Continue`.
  On EOF, signals `Fallback` → `<|>` chains to `done`.
- `done`: extracts the accumulated counts, signals `Done` → `loopAlt` stops.

Each sub-stage (`hIsEOF`, `hGetLine`, `getWords`) is wrapped with `meterNamed`,
accumulating per-stage timings in a `Map String [Nanos]` threaded through the state.

## repl

```haskell
>>> import Words
>>> (m, counts) <- wordCountSignal
>>> Map.size counts
2921
>>> let p50 xs = sort xs !! (length xs `div` 2)
>>> mapM_ (\(n,ts) -> putStrLn $ n ++ ": " ++ show (length ts) ++ " calls, p50=" ++ show (p50 ts)) (Map.toList m)
getWords: 3384 calls, p50=167
hGetLine: 3384 calls, p50=333
hIsEOF: 3385 calls, p50=208
```

## vs process pipeline

| approach | branching | metering | tensor |
|----------|-----------|----------|--------|
| `wordCountLineByLineMetered` | `bool` on hIsEOF | `meterNamed` per stage | Kleisli `(,)` (explicit state) |
| `wordCountSignal` | `Continue`/`Fallback`/`Done` + `<|>` | `meterNamed` per stage | Kleisli `(,)` (explicit state) |
| `Knot` with `Either` | `Left`/`Right` in step function | blocked (meterK breaks Either shape) | `Either` (trace iteration) |

The `Signal` approach separates "keep looping" from "try alternative" from "done"
— three distinct control signals, not two conflated into `Left`. The `<|>`
combinator chains stages without nesting conditionals. Per-stage metering works
because nothing wraps the output in `(Nanos, ...)` — the measurement map rides
alongside as state.

## relation to circuits

`Signal`/`<|>`/`loopAlt` live entirely in `Kleisli IO`. They don't use `Knot`,
`Either`, or `Trace`. The loop is explicit recursion + state threading via `(,)`.
Circuit's role here is providing `meterK timeM` (via `meterNamed`) — the metering
infrastructure. The loop structure is Kleisli-native.

For Circuit-native looping with metering, the path is: use `(,)` tensor + `ambient`
to thread the measurement map + explicit `hIsEOF`/`hGetLine` in the Kleisli.
The `<|>` combinator translates naturally: `ambient` for state threading, `Lift`
for each metered sub-stage, and the Kleisli-level `if eof then ... else ...` for
the Fallback branch.

## verified

- [x] per-stage metering with three-signal branching
- [x] 2921 unique words (matches all other pipelines)
- [ ] Circuit-native version with ambient + (,) tensor
