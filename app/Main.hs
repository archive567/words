-- | Word-count pipeline, producer-consumer style.
--
-- No closure-captured Handles. No IORef. The Handle is an explicit wire:
-- produced by openf, threaded through the Either-trace loop, and consumed
-- by the close stage.
module Main where

import Circuit
import Circuit.Meter (meterAction)
import Circuit.Meter.Time (Nanos, reifyC, timeM)
import Control.Arrow (Kleisli (..), runKleisli, second)
import Control.Category ((>>>))
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
-- Circuit primitives — payload-neutral, no closures
-- ---------------------------------------------------------------------------

openf :: Circuit (Kleisli IO) t FilePath Handle
openf = Lift (Kleisli (\fp -> openFile fp ReadMode))

closef :: Circuit (Kleisli IO) t Handle ()
closef = Lift (Kleisli hClose)

-- ---------------------------------------------------------------------------
-- Loop body — Either tensor, Handle rides the feedback wire
-- ---------------------------------------------------------------------------

readAndCount :: Circuit (Kleisli IO) Either Handle (Handle, Map String Int)
readAndCount = Knot (Kleisli step)
  where
    step (Left (h, acc)) =
      hIsEOF h >>= bool
        (hGetLine h >>= \line -> pure (Left (h, foldCounts (noEmpties (lowerWords (splitWords line))) acc)))
        (pure (Right (h, acc)))
    step (Right h) =
      pure (Left (h, Map.empty))

-- ---------------------------------------------------------------------------
-- Pipeline — open → read+count → format → close
-- ---------------------------------------------------------------------------

wordPipeline :: Circuit (Kleisli IO) Either FilePath String
wordPipeline =
  openf
    >>> readAndCount
    >>> Lift (Kleisli (\(h, m) -> hClose h >> pure (fmtTable m)))

wordCount :: FilePath -> IO ()
wordCount path = putStr =<< runKleisli (reify wordPipeline) path

-- ---------------------------------------------------------------------------
-- Whole-pipeline metering
-- ---------------------------------------------------------------------------

perfTest :: FilePath -> IO ()
perfTest path = do
  (t, output) <- runKleisli (reifyC (meterAction timeM (reify wordPipeline))) path
  let ms = fromIntegral t / 1_000_000 :: Double
  putStrLn $ " wall: " <> show ms <> " ms"
  putStr output

-- ---------------------------------------------------------------------------
-- second' experiment — thread a String tag through the pipeline
-- ---------------------------------------------------------------------------

demoSecond :: FilePath -> IO ()
demoSecond path = do
  (tag, output) <- runKleisli (second (reify wordPipeline)) ("tag-value", path)
  putStrLn $ "tag: " <> tag
  putStr output

-- ---------------------------------------------------------------------------
-- Instrumented run — per-stage metering, no IORef
--
-- Each stage is metered individually and the timings are reported directly.
-- ---------------------------------------------------------------------------

fmtMs :: Nanos -> String
fmtMs n =
  let ms = fromIntegral n / 1_000_000 :: Double
   in if ms < 0.001 then "<0.001ms" else showFFloat (Just 3) ms "ms"

timedRun :: FilePath -> IO ()
timedRun path = do
  -- stage 1: open
  (tOpen, h) <- runKleisli (reifyC (meterAction timeM (reify (openf :: Circuit (Kleisli IO) (,) FilePath Handle)))) path

  -- stage 2: read + count
  (tRead, (h', m)) <- runKleisli (reifyC (meterAction timeM (reify readAndCount))) h

  -- stage 3: format (pure, WHNF)
  let output = fmtTable m
  (tFmt, _) <- runKleisli (reifyC (meterAction timeM (Kleisli (pure . const ())))) output

  -- stage 4: close + print
  (tPrint, ()) <- runKleisli (reifyC (meterAction timeM (Kleisli (\s -> hClose h' >> putStr s)))) output

  putStrLn $ "open:  " <> fmtMs tOpen
  putStrLn $ "read:  " <> fmtMs tRead
  putStrLn $ "fmt:   " <> fmtMs tFmt
  putStrLn $ "print: " <> fmtMs tPrint

main :: IO ()
main = do
  wordCount "other/alice.md"
  putStrLn ""
  perfTest "other/alice.md"
  putStrLn ""
  demoSecond "other/alice.md"
  putStrLn ""
  timedRun "other/alice.md"
