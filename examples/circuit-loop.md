# circuit-loop ⟜ line-by-line as Circuit Knot

The line-by-line loop expressed as a `Circuit (Kleisli IO) Either` with `Knot`.
The `Either` tensor encodes iteration: `Left` = feedback (continue), `Right` = exit (done).

## the loop

State `a = (Handle, Map String Int)` rides the `Left` channel. Input `b = Handle`,
output `c = Map String Int`.

```haskell
import Words (getWords, (|>), (.>))
import Circuit.Circuit (Circuit(..), reify)
import Control.Arrow (Kleisli(..), runKleisli)
import System.IO (Handle, hIsEOF, hGetLine, withFile, IOMode(ReadMode))
import qualified Data.Map.Strict as Map

:{
let step = Kleisli $ \case
      Left (h, acc) -> do        -- continue: process next line
        eof <- hIsEOF h
        if eof then pure (Right acc)
        else do
          line <- hGetLine h
          let acc' = line |> getWords |> foldl' (\m w -> Map.insertWith (+) w 1 m) acc
          pure (Left (h, acc'))
      Right h -> do               -- initial: start looping
        eof <- hIsEOF h
        if eof then pure (Right Map.empty)
        else do
          line <- hGetLine h
          let acc' = line |> getWords |> foldl' (\m w -> Map.insertWith (+) w 1 m) Map.empty
          pure (Left (h, acc'))
:}

-- Knot ties the feedback channel
let loop = Knot step

-- reify pins the tensor
let reify' = reify :: Circuit (Kleisli IO) Either Handle (Map String Int) -> Kleisli IO Handle (Map String Int)

-- >>> counts <- withFile "other/alice.md" ReadMode $ \h -> runKleisli (reify' loop) h
-- >>> Map.size counts
-- 2921
```

## vs process style

| process (Kleisli `>->`)         | circuit (`Knot`)                     |
|---------------------------------|--------------------------------------|
| `loop h acc` — explicit recurse | `Knot step` — feedback via `Either`  |
| `bool` branching                | `Left`/`Right` in step function      |
| `>->` composes stages           | `step` is one big function           |
| `meterNamed` wraps each stage   | `meterK` breaks Knot signature       |

## the metering gap

`meterK timeM` wraps output in `(Nanos, ...)`, but `Knot` needs `Either a c`.
To meter individual stages inside a Knot, one of:

- **ambient**: thread a measurement Map alongside via the `(,)` tensor, updating it
  at each substage. The Map rides `ambient` while the loop iterates via `Either`.
- **meterS**: a general state-channel meter (`t -> m -> m`) that fits the Knot
  input/output shape without changing the Either structure.

## verified

- [x] Knot loop produces 2,921 unique words (matches process pipeline)
- [ ] per-stage metered Knot loop
- [ ] file bracket: `openFile ◅ knotLoop ▻ closeFile`
