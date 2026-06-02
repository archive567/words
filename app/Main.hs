-- | Word-count pipeline as a verified Circuit.
--
-- No do-notation. Only `>>=`, `>=>`, `>>>`, and point-free composition.
-- Built from `Knot` (feedback loop over `Either`), `Lift` (pure stages),
-- and `Compose` (via `>>>`).
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import Circuit
import Circuit.Meter (meterAction)
import Circuit.Meter.Time (meterIO, Nanos, reifyC, timeM)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Category ((>>>))
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.Bool (bool)
import Data.Char (toLower)
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
-- Circuit-native bracketing
--
-- withFileC wraps a file handle around a Circuit, lifting withFile into
-- the composition rather than wrapping runKleisli from the outside.
-- ---------------------------------------------------------------------------

withFileC :: Trace (Kleisli IO) t => FilePath -> IOMode -> (Handle -> Circuit (Kleisli IO) t a b) -> Circuit (Kleisli IO) t a b
withFileC path mode f = Lift (Kleisli (\a -> withFile path mode (\h -> runKleisli (reify (f h)) a)))

wordCount :: FilePath -> IO ()
wordCount path = runKleisli (reify (withFileC path ReadMode wordPipeline)) ()

-- ---------------------------------------------------------------------------
-- Whole-pipeline metering
-- ---------------------------------------------------------------------------

perfTest :: FilePath -> IO ()
perfTest path = do
  (t, ()) <- runKleisli (reifyC (meterAction timeM (Kleisli (\_ -> wordCount path)))) ()
  let ms = fromIntegral t / 1_000_000 :: Double
  putStrLn $ " wall: " <> show ms <> " ms"

-- ---------------------------------------------------------------------------
-- Instrumented run — real timings for a mermaid diagram
--
-- TimingLog is threaded through the loop state instead of an IORef.
-- The metering pattern is factored into meterIO' / meterPure' / meterList'.
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

-- | Meter an IO action and accumulate the timing.
meterIO' :: (a -> IO b) -> (TimingLog -> Nanos -> TimingLog) -> TimingLog -> a -> IO (TimingLog, b)
meterIO' f upd tlog a = do
  (t, b) <- runKleisli (reifyC (meterAction timeM (Kleisli f))) a
  pure (upd tlog t, b)

-- | Meter a pure function (WHNF) and accumulate the timing.
meterPure' :: (a -> b) -> (TimingLog -> Nanos -> TimingLog) -> TimingLog -> a -> IO (TimingLog, b)
meterPure' f upd tlog a = do
  (t, b) <- runKleisli (reifyC (meterAction timeM (Kleisli (evaluate . f)))) a
  pure (upd tlog t, b)

-- | Meter a lazy-list-producing function (full spine) and accumulate.
meterList' :: NFData b => (a -> [b]) -> (TimingLog -> Nanos -> TimingLog) -> TimingLog -> a -> IO (TimingLog, [b])
meterList' f upd tlog a = do
  (t, b) <- runKleisli (reifyC (meterAction timeM (Kleisli (evaluate . force . f)))) a
  pure (upd tlog t, b)

-- | Loop body that carries TimingLog in the feedback state.
loopBodyLogged
  :: Handle
  -> Kleisli IO (Either (TimingLog, Map String Int) ()) (Either (TimingLog, Map String Int) (TimingLog, Map String Int))
loopBodyLogged h = Kleisli (either go (const (go (emptyLog, Map.empty))))
  where
    go (tlog, acc) = hIsEOF h >>= bool (step (tlog, acc)) (pure (Right (tlog, acc)))
    step (tlog, acc) = do
      (tlog, eof) <- meterIO' (\_ -> hIsEOF h) (\l t -> l { tIsEOF = tIsEOF l + t, nIterations = nIterations l + 1 }) tlog ()
      if eof
        then pure (Right (tlog, acc))
        else do
          (tlog, line) <- meterIO' (\_ -> hGetLine h) (\l t -> l { tHGetLine = tHGetLine l + t, nLinesRead = nLinesRead l + 1 }) tlog ()
          (tlog, ws) <- meterList' words (\l t -> l { tWords = tWords l + t }) tlog line
          (tlog, wsLower) <- meterList' (map (map toLower)) (\l t -> l { tLower = tLower l + t }) tlog ws
          (tlog, wsFiltered) <- meterList' (filter (not . null)) (\l t -> l { tFilter = tFilter l + t }) tlog wsLower
          (tlog, acc') <- meterPure' (flip foldCounts acc) (\l t -> l { tFold = tFold l + t }) tlog wsFiltered
          pure (Left (tlog, acc'))

-- | Post-processing stages, each metered and logging.
postProcess :: TimingLog -> Map String Int -> IO (TimingLog, String)
postProcess tlog m = do
  (tlog, list) <- meterPure' Map.toList (\l t -> l { tToList = tToList l + t }) tlog m
  (tlog, sorted) <- meterPure' (sortOn (Down . snd)) (\l t -> l { tSort = tSort l + t }) tlog list
  (tlog, top5) <- meterPure' (take 5) (\l t -> l { tTake = tTake l + t }) tlog sorted
  (tlog, output) <- meterPure' fmtTable (\l t -> l { tFmt = tFmt l + t }) tlog top5
  (tlog, ()) <- meterIO' putStr (\l t -> l { tPutStr = tPutStr l + t }) tlog output
  pure (tlog, output)

timedRun :: FilePath -> IO ()
timedRun path = withFile path ReadMode $ \h -> do
  (tlog, result) <- runKleisli (trace (loopBodyLogged h)) ()
  (tlog', _) <- postProcess tlog result
  putStrLn (mermaidDiagram tlog')

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
