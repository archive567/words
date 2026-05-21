# words

R&D journal. Word counting as a circuits laboratory.

## entry 1 — two pipelines, two resource strategies

The same computation (word frequency → top 5), two ways to read the file.

### A: all-at-once

`readFile` slurps the whole file into a String. Simple, but the entire
file lives in memory at once. Fine for Alice (170KB), but it doesn't
scale — and it doesn't compose with other resource-bound stages.

```haskell
wordCountAllAtOnce :: Int -> FilePath -> IO ()
wordCountAllAtOnce n path = do
  contents <- readFile path
  printTopN n contents

wordCountAllAtOnceFile :: IO ()
wordCountAllAtOnceFile = wordCountAllAtOnce 5 "other/alice.md"
```

### B: line-by-line

Opens the file, reads one line at a time via `hGetLine`, accumulates
counts into a Map. The handle is the resource — it's explicitly opened
and closed (`withFile`). This is closer to a streaming pipeline: each
line is processed and discarded before the next is read.

```haskell
processLineByLine :: Handle -> Map String Int -> IO (Map String Int)
processLineByLine h acc = do
  eof <- hIsEOF h
  if eof
    then pure acc
    else do
      line <- hGetLine h
      let chunkCounts = countWords line
      processLineByLine h (mergeCounts acc chunkCounts)

wordCountLineByLine :: Int -> FilePath -> IO ()
wordCountLineByLine n path = do
  counts <- withFile path ReadMode (`processLineByLine` Map.empty)
  printFrequencies (topN n counts)

wordCountLineByLineFile :: IO ()
wordCountLineByLineFile = wordCountLineByLine 5 "other/alice.md"
```

Both produce the same output. The difference is *how the file is held*.

### what's next

The `withFile` / `processLineByLine` pattern is a resource bracket in
disguise — open, use, close. But it's not compositional: the handle is
threaded through an explicit recursive loop, not through the category.

The question for entry 2: can we make the file bracket a first-class
Circuit combinator?
