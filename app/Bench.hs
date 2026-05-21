-- | Compiled benchmark for word-counting pipelines.
module Main where

import Circuit.Perf (timesK)
import Circuit.Perf.Time (ticks, timeM)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.List (sort)
import Words (countWords, wordCountAllAtOnceFile, wordCountLineByLineFile)

stats :: [Integer] -> String
stats ts =
  let sorted = sort ts
      n = length sorted
      mn = minimum ts
      p50 = sorted !! (n `div` 2)
      avg = sum ts `div` fromIntegral n
   in "runs: " ++ show n
        ++ "  min: " ++ show mn
        ++ " ns  p50: " ++ show p50
        ++ " ns  mean: " ++ show avg
        ++ " ns"

main :: IO ()
main = do
  -- pure counting (no I/O)
  contents <- evaluate . force =<< readFile "other/alice.md"
  (tsP, _) <- ticks 500 countWords contents
  putStrLn "=== pure counting (countWords) ==="
  putStrLn $ stats tsP

  -- full pipeline: all-at-once
  putStrLn ""
  putStrLn "=== all-at-once ==="
  (tsA, _) <- runKleisli (timesK 100 timeM (Kleisli (const wordCountAllAtOnceFile))) ()
  putStrLn ""
  putStrLn $ stats tsA

  -- full pipeline: line-by-line
  putStrLn ""
  putStrLn "=== line-by-line ==="
  (tsL, _) <- runKleisli (timesK 100 timeM (Kleisli (const wordCountLineByLineFile))) ()
  putStrLn ""
  putStrLn $ stats tsL
