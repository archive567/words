-- | Word-count pipeline as a verified Circuit.
--
-- No do-notation. Only `>>=`, `>=>`, `>>>`, and point-free composition.
-- Built from `Knot` (feedback loop over `Either`), `Lift` (pure stages),
-- and `Compose` (via `>>>`).
module Main where

import Circuit
import Circuit.Meter (meterA)
import Circuit.Meter.Time (meterIO, Nanos, timeM)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Category ((>>>))
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Numeric (showFFloat)
import System.IO (Handle, IOMode (ReadMode), hGetLine, hIsEOF, withFile)

-- ---------------------------------------------------------------------------
-- Pure components (each has one job, testable in isolation)
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

assocList :: Map String Int -> [(String, Int)]
assocList = Map.toList

sortFreq :: [(String, Int)] -> [(String, Int)]
sortFreq = sortOn (Down . snd)

topN :: Int -> [(String, Int)] -> [(String, Int)]
topN n = take n

fmtRow :: (String, Int) -> String
fmtRow (w, c) = w <> ": " <> show c

fmtTable :: [(String, Int)] -> String
fmtTable = unlines . map fmtRow

-- ---------------------------------------------------------------------------
-- IO components
-- ---------------------------------------------------------------------------

hGetLineIO :: Handle -> IO String
hGetLineIO = hGetLine

-- ---------------------------------------------------------------------------
-- Metered IO (component-level timing)
--
-- meterIO is now polymorphic in the tensor t, so the resulting Kleisli
-- can be lifted into Either-based Knot loops as well as (,) pipelines.
-- ---------------------------------------------------------------------------

meteredHGetLine :: Kleisli IO Handle (Nanos, String)
meteredHGetLine = reify (meterIO hGetLineIO :: Circuit (Kleisli IO) Either Handle (Nanos, String))

-- ---------------------------------------------------------------------------
-- Loop body — the per-line iteration wrapped in Either
--
--   Left acc  → continue reading lines
--   Right acc → done, emit result
-- ---------------------------------------------------------------------------

loopBody
  :: Handle
  -> Kleisli IO (Either (Map String Int) ()) (Either (Map String Int) (Map String Int))
loopBody h = Kleisli (either go (const (go Map.empty)))
  where
    go acc = hIsEOF h >>= bool (step acc) (pure (Right acc))
    step acc =
      runKleisli meteredHGetLine h
        >>= pure . Left . flip foldCounts acc . noEmpties . lowerWords . splitWords . snd

-- ---------------------------------------------------------------------------
-- The pipeline
--
-- Knot holds the loop open. Lift bookends it with format-and-print.
-- ---------------------------------------------------------------------------

wordPipeline
  :: Handle
  -> Circuit (Kleisli IO) Either () ()
wordPipeline h =
  Knot (loopBody h)
    >>> Lift (Kleisli (putStr . fmtTable . topN 5 . sortFreq . assocList))

-- ---------------------------------------------------------------------------
-- Running it
-- ---------------------------------------------------------------------------

wordCount :: FilePath -> IO ()
wordCount path = withFile path ReadMode $ \h -> runKleisli (reify (wordPipeline h)) ()

-- ---------------------------------------------------------------------------
-- Whole-pipeline metering
--
-- meterIO wraps the entire IO action; reify (meterIO f) gives a Kleisli
-- that can be run directly.
-- ---------------------------------------------------------------------------

perfTest :: FilePath -> IO ()
perfTest path = do
  (t, ()) <- runKleisli (meterA timeM (Kleisli (\_ -> wordCount path))) ()
  let ms = fromIntegral t / 1_000_000 :: Double
  putStrLn $ " wall: " <> show ms <> " ms"

-- ---------------------------------------------------------------------------
-- Instrumented run — real timings for a mermaid diagram
-- ---------------------------------------------------------------------------

data TimingLog = TimingLog
  { tIsEOF :: !Nanos,
    tHGetLine :: !Nanos,
    tWords :: !Nanos,
    tLower :: !Nanos,
    tFilter :: !Nanos,
    tFold :: !Nanos,
    tToList :: !Nanos,
    tSort :: !Nanos,
    tTake :: !Nanos,
    tFmt :: !Nanos,
    tPutStr :: !Nanos,
    nLinesRead :: !Int,
    nIterations :: !Int
  }

emptyLog :: TimingLog
emptyLog = TimingLog 0 0 0 0 0 0 0 0 0 0 0 0 0

metered :: (a -> IO b) -> Kleisli IO a (Nanos, b)
metered f = meterA timeM (Kleisli f)

-- | Meter a pure function, forcing result to WHNF inside the bracket.
-- For functions that already return strict data (like foldl'), WHNF is
-- sufficient.  For lazy lists, use 'meteredList' which forces the spine.
meteredPure :: (a -> b) -> Kleisli IO a (Nanos, b)
meteredPure f = meterA timeM (Kleisli (evaluate . f))

-- | Meter a lazy-list-producing function, forcing the full spine.
meteredList :: NFData b => (a -> [b]) -> Kleisli IO a (Nanos, [b])
meteredList f = meterA timeM (Kleisli (evaluate . force . f))

timedRun :: FilePath -> IO ()
timedRun path = withFile path ReadMode $ \h -> do
  ref <- newIORef emptyLog

  let step acc = do
        (t, eof) <- runKleisli (metered (\_ -> hIsEOF h)) ()
        modifyIORef ref $ \r -> r { tIsEOF = tIsEOF r + t, nIterations = nIterations r + 1 }
        if eof
          then pure (Right acc)
          else do
            (t, line) <- runKleisli (metered (\_ -> hGetLine h)) ()
            modifyIORef ref $ \r -> r { tHGetLine = tHGetLine r + t, nLinesRead = nLinesRead r + 1 }

            (t, ws) <- runKleisli (meteredList words) line
            modifyIORef ref $ \r -> r { tWords = tWords r + t }

            (t, wsLower) <- runKleisli (meteredList (map (map toLower))) ws
            modifyIORef ref $ \r -> r { tLower = tLower r + t }

            (t, wsFiltered) <- runKleisli (meteredList (filter (not . null))) wsLower
            modifyIORef ref $ \r -> r { tFilter = tFilter r + t }

            (t, acc') <- runKleisli (meteredPure (flip foldCounts acc)) wsFiltered
            modifyIORef ref $ \r -> r { tFold = tFold r + t }

            pure (Left acc')

      body = Kleisli $ \case
        Right () -> runKleisli body (Left Map.empty)
        Left acc -> step acc

  result <- runKleisli (trace body) ()

  (t, list) <- runKleisli (meteredPure Map.toList) result
  modifyIORef ref $ \r -> r { tToList = tToList r + t }

  (t, sorted) <- runKleisli (meteredPure (sortOn (Down . snd))) list
  modifyIORef ref $ \r -> r { tSort = tSort r + t }

  (t, top5) <- runKleisli (meteredPure (take 5)) sorted
  modifyIORef ref $ \r -> r { tTake = tTake r + t }

  (t, output) <- runKleisli (meteredPure fmtTable) top5
  modifyIORef ref $ \r -> r { tFmt = tFmt r + t }

  (t, ()) <- runKleisli (metered putStr) output
  modifyIORef ref $ \r -> r { tPutStr = tPutStr r + t }

  l <- readIORef ref
  putStrLn (mermaidDiagram l)

fmtMs :: Nanos -> String
fmtMs n =
  let ms = fromIntegral n / 1_000_000 :: Double
   in if ms < 0.001 then "<0.001ms" else showFFloat (Just 3) ms "ms"

avgMs :: Nanos -> Int -> String
avgMs t n = fmtMs (t `div` fromIntegral (max 1 n))

mermaidDiagram :: TimingLog -> String
mermaidDiagram l =
  let n = max 1 (nLinesRead l)
   in unlines
        [ "flowchart TD",
          "    B[\"Right ()\"] --> C[\"init Map.empty\"]",
          "        C --> D{\"hIsEOF ? ⏱ " <> avgMs (tIsEOF l) (nIterations l) <> "\"}",
          "        D -->|\"no\"| E[\"hGetLine ⏱ " <> avgMs (tHGetLine l) n <> "\"]",
          "        E --> F[\"words ⏱ " <> avgMs (tWords l) n <> "\"]",
          "        F --> G[\"map toLower ⏱ " <> avgMs (tLower l) n <> "\"]",
          "        G --> H[\"filter (not . null) ⏱ " <> avgMs (tFilter l) n <> "\"]",
          "        H --> I[\"foldl' insertCount ⏱ " <> avgMs (tFold l) n <> "\"]",
          "        I --> J[\"Left\"]",
          "        J -.->|\"feedback\"| D",
          "",
          "    D -->|\"yes\"| K[\"Map.toList ⏱ " <> fmtMs (tToList l) <> "\"]",
          "    K --> L[\"sortOn Down ⏱ " <> fmtMs (tSort l) <> "\"]",
          "    L --> M[\"take 5\"]",
          "    M --> N[\"fmtRow\"]",
          "    N --> O[\"unlines\"]",
          "    O --> P[\"putStr ⏱ total: " <> fmtMs (tPutStr l) <> "\"]"
        ]

main :: IO ()
main = do
  wordCount "other/alice.md"
  putStrLn ""
  perfTest "other/alice.md"
  putStrLn ""
  timedRun "other/alice.md"
