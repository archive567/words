# iter-loop ⟜ Either-trace iteration on the cartesian tensor

`Iter s r = Loop s | Exit r` is the `Either`-trace iteration pattern,
manually encoded for the @(,)@ tensor.

`Loop` feeds back (like `Left` in the `Either` trace).  `Exit` terminates
(like `Right`).  `loopIter` is what `trace` does for `Either`, rebuilt by
hand for `(,)`.

```haskell
import Circuit (Iter (..), loopIter)

data Iter s r = Loop s | Exit r

loopIter :: Monad m => Kleisli m s (Iter s r) -> Kleisli m s r
-- feeds Loop back, returns on Exit
```

Because it works for any `Monad m`, it applies to pure functions
(`Kleisli Identity`, i.e. `(->)`) as well as effectful pipelines
(`Kleisli IO`).

## the metering gap

`meterK timeM` wraps output in `(Nanos, b)`.  This fixes the tensor to
`(,)`.  `Knot` with `Either` then breaks because `Either` and `(,)` are
different shapes.

`Iter` bridges the gap: the state rides `(,)` as usual, and `Loop`/`Exit`
provide the control flow that `Either` would have given for free.

## word-counting loop (sketch)

```haskell
import Circuit (Iter (..), loopIter)
import Control.Arrow (Kleisli (..), runKleisli)

let step = Kleisli $ \(h, acc) -> do
      eof <- hIsEOF h
      if eof
        then pure (Exit acc)
        else do
          line <- hGetLine h
          let acc' = foldl' (\m w -> Map.insertWith (+) w 1 m) acc (getWords line)
          pure (Loop (h, acc'))

-- loopIter closes the iteration on (,)
counts <- withFile "other/alice.md" ReadMode $ \h ->
  runKleisli (loopIter step) (h, Map.empty)
```

Per-stage metering works by threading a measurement map through the state
— the same state wire that `(,)` already carries.  No output wrapping, no
`Either` shape constraint.

## vs process pipeline

| approach | branching | loop mechanism | tensor |
|----------|-----------|----------------|--------|
| `wordCountLineByLine` | `bool` on hIsEOF | explicit recursion | plain `IO` |
| `Iter` + `loopIter` | `Loop`/`Exit` | `loopIter` recurses | `Kleisli` `(,)` |
| `Knot` with `Either` | `Left`/`Right` | `trace` iterates | `Either` (trace) |

`Iter` occupies the middle row: it gives you `Either`-style control flow
while keeping the `(,)` tensor intact.

## relation to circuits

`Iter` is not a `Circuit` combinator.  It is a **bridge**: the `Either`
trace gives iteration for free via `Knot`, but breaks when outputs are
wrapped.  `Iter` rebuilds the same control flow by hand on `(,)`, keeping
the state type intact.

For genuine `Circuit`-native iteration, `Knot` + `Either` is still the
right tool.  `Iter` is the escape hatch when `(,)` is fixed by context.

## verified

- [x] `Iter` encodes `Either`-trace iteration on `(,)`
- [ ] per-stage metered loop using `Iter`
- [ ] Circuit-native version with `ambient` + `(,)` tensor
