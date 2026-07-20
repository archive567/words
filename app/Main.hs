-- | Word-count pipeline with stopwatch/interval metering.
--
-- The timing log is a first-class wire: it travels alongside the payload on
-- the (,) tensor. Every composition point can be a named marker.
module Main where

import Circuit
import Circuit.Category ((.>))
import Circuit.Meter.Stopwatch
import Circuit.Meter.Time (Nanos, timeX)
import Control.Arrow (Kleisli (..), arr, first, runKleisli)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Numeric (showFFloat)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetLine, hIsEOF, openFile)

-- ---------------------------------------------------------------------------
-- Pure components
-- ---------------------------------------------------------------------------

splitWords :: String -> [String]
splitWords = words

lowerWords :: [String] -> [String]
lowerWords = map (map toLower)

noEmpties :: [String] -> [String]
noEmpties = filter (not . null)

insertCount :: Map String Int -> String -> Map String Int
insertCount m w = Map.insertWith (+) w (1 :: Int) m

foldCounts :: [String] -> Map String Int -> Map String Int
foldCounts = flip (foldl' insertCount)

fmtTable :: Map String Int -> String
fmtTable = unlines . map fmt . take 5 . sortOn (Down . snd) . Map.toList
  where
    fmt (w, c) = w <> ": " <> show c

-- ---------------------------------------------------------------------------
-- Loop primitives — payload-neutral, no closures
-- ---------------------------------------------------------------------------

openf :: Loop t (Kleisli IO) FilePath Handle
openf = Lift (Kleisli (`openFile` ReadMode))

closef :: Loop t (Kleisli IO) Handle ()
closef = Lift (Kleisli hClose)

-- | Close the Handle and keep the paired value.
post :: Kleisli IO (Handle, a) a
post = first (run (closef :: Loop (,) (Kleisli IO) Handle ())) .> arr snd

-- | Force the map component of the loop result so the read stage pays for
-- the counting work rather than deferring it to formatting.
forceMap :: Kleisli IO (Handle, Map String Int) (Handle, Map String Int)
forceMap = Kleisli $ \(h, m) -> do
  !m' <- evaluate (force m)
  pure (h, m')

formatf :: Loop t (Kleisli IO) (Map String Int) String
formatf = Lift (Kleisli (pure . fmtTable))

-- | Forced variant so the stopwatch measures the actual formatting work.
formatfForced :: Loop t (Kleisli IO) (Map String Int) String
formatfForced = Lift (Kleisli (evaluate . force . fmtTable))

-- ---------------------------------------------------------------------------
-- Loop body — Either tensor, Handle rides the feedback wire
-- ---------------------------------------------------------------------------

readAndCount :: Loop Either (Kleisli IO) Handle (Handle, Map String Int)
readAndCount = Knot (Kleisli step)
  where
    step (Left (h, acc)) =
      hIsEOF h
        >>= bool
          ( hGetLine h >>= \line ->
              let ws = splitWords line
                  ls = lowerWords ws
                  ns = noEmpties ls
                  acc' = foldCounts ns acc
               in pure (Left (h, acc'))
          )
          (pure (Right (h, acc)))
    step (Right h) =
      pure (Left (h, Map.empty))

-- ---------------------------------------------------------------------------
-- Stopwatch pipeline
-- ---------------------------------------------------------------------------

-- | Top-level word-count pipeline with interval markers.
wordPipeline :: Loop (,) (Kleisli IO) FilePath (String, Watches Nanos Nanos)
wordPipeline =
  start timeX "total"
    .> carryT (openf :: Loop (,) (Kleisli IO) FilePath Handle)
    .> lap timeX "opened"
    .> carryT readAndCount
    .> carry forceMap
    .> lap timeX "read"
    .> carry post
    .> lap timeX "closed"
    .> carryT (formatfForced :: Loop (,) (Kleisli IO) (Map String Int) String)
    .> stop timeX "total"

-- | Run the pipeline and print the word table plus the timing log.
wordCount :: FilePath -> IO ()
wordCount path = do
  (output, ws) <- runKleisli (run wordPipeline) path
  putStr output
  putStrLn ""
  putStrLn "interval timings:"
  mapM_ (\(name, ts) -> putStrLn $ "  " <> name <> ": " <> fmtMs (sum ts)) $ Map.toList (allLaps ws)

-- ---------------------------------------------------------------------------
-- Function-composition experiment
--
-- Compare timing when the pure word-processing functions are composed
-- (allowing GHC to fuse) versus when they are kept as separate Kleisli
-- stages (forcing the intermediate lists).
-- ---------------------------------------------------------------------------

-- | One line through the separate pure stages, each measured.
--
-- Work is forced only at the end, so most cost collapses into the final
-- 'sep' interval. The intermediate laps measure almost nothing.
separateStages :: Loop (,) (Kleisli IO) String ([String], Watches Nanos Nanos)
separateStages =
  start timeX "sep"
    .> carry (Kleisli (pure . splitWords))
    .> lap timeX "split"
    .> carry (Kleisli (pure . lowerWords))
    .> lap timeX "lower"
    .> carry (Kleisli (pure . noEmpties))
    .> carry (Kleisli (evaluate . force))
    .> stop timeX "sep"

-- | One line through the separate pure stages, forcing after each stage.
--
-- This defeats fusion and materialises every intermediate list, but it shows
-- where the time actually goes.
separateStagesForced :: Loop (,) (Kleisli IO) String ([String], Watches Nanos Nanos)
separateStagesForced =
  start timeX "sep"
    .> carry (Kleisli (pure . splitWords))
    .> carry (Kleisli (evaluate . force))
    .> lap timeX "split"
    .> carry (Kleisli (pure . lowerWords))
    .> carry (Kleisli (evaluate . force))
    .> lap timeX "lower"
    .> carry (Kleisli (pure . noEmpties))
    .> carry (Kleisli (evaluate . force))
    .> lap timeX "filter"
    .> stop timeX "sep"

-- | One line through the fully composed pure function.
fusedStage :: Loop (,) (Kleisli IO) String ([String], Watches Nanos Nanos)
fusedStage =
  start timeX "fus"
    .> carry (Kleisli (pure . noEmpties . lowerWords . splitWords))
    .> carry (Kleisli (evaluate . force))
    .> stop timeX "fus"

-- | Process a repeated line in three ways to expose laziness + fusion effects.
lineExperiment :: String -> IO ()
lineExperiment line = do
  putStrLn "\n--- function-composition experiment ---"
  let wordsPerRun = 100000
      longLine = unwords (replicate wordsPerRun line)
  (_, wsSep) <- runKleisli (run separateStages) longLine
  (_, wsSepF) <- runKleisli (run separateStagesForced) longLine
  (_, wsFus) <- runKleisli (run fusedStage) longLine
  putStrLn $ "input words: " <> show wordsPerRun
  putStrLn "separate (lazy, force at end):"
  mapM_ (\(name, ts) -> putStrLn $ "  " <> name <> ": " <> fmtMs (sum ts)) $ Map.toList (allLaps wsSep)
  putStrLn "separate (force after each stage):"
  mapM_ (\(name, ts) -> putStrLn $ "  " <> name <> ": " <> fmtMs (sum ts)) $ Map.toList (allLaps wsSepF)
  putStrLn "fused (single force):"
  mapM_ (\(name, ts) -> putStrLn $ "  " <> name <> ": " <> fmtMs (sum ts)) $ Map.toList (allLaps wsFus)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

fmtMs :: Nanos -> String
fmtMs n =
  let ms = fromIntegral n / 1_000_000 :: Double
   in if ms < 0.001 then "<0.001ms" else showFFloat (Just 3) ms "ms"

main :: IO ()
main = do
  wordCount "other/alice.md"
  lineExperiment "The quick brown fox jumps over the lazy dog"
