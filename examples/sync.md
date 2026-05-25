# Sync

A recipe for merging two asynchronous streams.

## The idea

You have two streams — one of `a`, one of `b`. They arrive at different rates,
in no particular order. Tag each element with its source and interleave:

```haskell
sync :: [a] -> [b] -> [Either a b]
```

The result is a single timeline where every event is stamped `Left` (from the
`a` stream) or `Right` (from the `b` stream). The two streams are now
synchronized — not by blocking or waiting, but by sharing a common index.

## The dual

```haskell
partition :: [Either a b] -> ([a], [b])
partition = foldr (\e (as, bs) -> case e of Left a -> (a:as, bs); Right b -> (as, b:bs)) ([], [])
```

`partition` recovers the original streams, preserving order within each source.
`sync` followed by `partition` is identity (up to interleaving).

## The spectrum of shapes

`[Either a b]` is the general case: any finite sequence of tagged events.
Two well-known restrictions on the list shape recover the standard synchronous
tensors:

| Restriction | Shape | Meaning |
|---|---|---|
| **Either** | `[Left a]` or `[Right b]` | Exactly one event, from one source. Isomorphic to `Either a b`. |
| **( , )** | `[Left a, Right b]` or `[Right b, Left a]` | Exactly one event from each source, in some order. Isomorphic to `(a, b)` up to permutation. |
| **Sync** | `[Either a b]` | Any sequence. The free case. |

In the `Either` restriction the two streams are mutually exclusive — you get
one or the other, never both, never neither.

In the `(,)` restriction the two streams are coterminous — you get exactly one
from each, delivered as a pair of events (order is presentational, not
semantic).

In the general `Sync` case the streams are asynchronous — any number from
either, in any order, including zero.

## Why this matters

`(,)` says "both, now". `Either` says "one or the other, now". `[Either a b]`
says "whatever arrives, in the order it arrives". The latter is the natural
setting for metering, logging, tracing, and any other problem where two
independent producers need to share a single consumer.

The empty list `[]` is the unit: no events from either stream. Its meaning is
context-dependent — not yet started, filtered out, or deliberately skipped.
