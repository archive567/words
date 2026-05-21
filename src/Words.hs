-- | Word counting as a circuits laboratory.
--
-- R&D journal. Left-to-right process style with performance annotations.
-- Per-stage timings from repl, 3,384 lines of alice.md.
module Words
  ( wordCountAllAtOnceFile,
    wordCountLineByLineFile,

    -- * Pure stages
    countWords,
    getWords,
    formatTop,

    -- * Process combinators
    (|>),
    (.>),
    (>->),

    -- * Metered pipeline (for perf journal)
    wordCountLineByLineMetered,
    meterNamed,
  )
where

import Control.Arrow (Kleisli (..), runKleisli, (>>>))
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Function ((&))
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import System.IO (hGetLine, hIsEOF, withFile, IOMode (ReadMode))

import Circuit.Perf (Nanos, meterK)
import Circuit.Perf.Time (timeM)

-- | Forward application: @x |> f = f x@.
infixl 1 |>
(|>) :: a -> (a -> b) -> b
(|>) = (&)

-- | Forward composition: @f .> g = g . f@.
infixr 1 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = (>>>)

-- | Forward Kleisli composition: @f >-> g@ pipes the result of @f@ into @g@.
infixr 1 >->
(>->) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >-> g = \x -> f x >>= g

-- $setup
-- >>> import Words

-- ---------------------------------------------------------------------------
-- Pure stages (left-to-right)
-- ---------------------------------------------------------------------------

-- | Extract words from a string: split, keep a-z, lowercase, drop empties.
--
-- >>> "Hello, World!" |> getWords
-- ["hello","world"]
getWords :: String -> [String]
getWords = words .> map (map toLower . filter (`elem` ['a' .. 'z'])) .> filter (not . null)

-- | Count word frequencies from a string.
--
-- >>> "a b a" |> countWords |> Map.toList
-- [("a",2),("b",1)]
countWords :: String -> Map String Int
countWords = getWords .> map (\w -> (w, 1)) .> Map.fromListWith (+)

-- | Format the top N word frequencies.
--
-- >>> let m = Map.fromList [("the",10),("a",5),("cat",3)]
-- >>> putStr $ formatTop 2 m
-- the: 10
-- a: 5
formatTop :: Int -> Map String Int -> String
formatTop n = Map.toList .> sortOn (Down . snd) .> take n .> map (\(w, c) -> w ++ ": " ++ show c) .> unlines

-- ---------------------------------------------------------------------------
-- Pipeline A: all-at-once
-- ---------------------------------------------------------------------------

-- | All-at-once pipeline: read entire file, count words, print top 5.
--
-- >>> wordCountAllAtOnceFile
-- the: 1523
-- and: 779
-- to: 720
-- a: 616
-- she: 501
wordCountAllAtOnceFile :: IO ()
wordCountAllAtOnceFile = do
  contents <- readFile "other/alice.md"
  contents |> (countWords .> formatTop 5) |> putStr

-- ---------------------------------------------------------------------------
-- Pipeline B: line-by-line (resource-constrained)
-- ---------------------------------------------------------------------------

-- | Line-by-line pipeline: read one line at a time, accumulate, print top 5.
--
-- >>> wordCountLineByLineFile
-- the: 1523
-- and: 779
-- to: 720
-- a: 616
-- she: 501
wordCountLineByLineFile :: IO ()
wordCountLineByLineFile =
  withFile "other/alice.md" ReadMode $ \h -> do
    freqs <- loop h Map.empty
    freqs |> formatTop 5 |> putStr
  where
    loop h acc =
      h |> hIsEOF >>= bool cont (pure acc)
      where
        cont =
          h |> ( hGetLine                                                       -- 291 ns p50 (3,384 calls)
                  >-> pure . ( getWords                                          -- 125 ns p50 (3,384 calls)
                               .> foldl' (\m w -> m |> Map.insertWith (+) w 1) acc
                             )
                  >-> loop h
               )

-- ---------------------------------------------------------------------------
-- Metered line-by-line: per-stage timing accumulated in a Map
-- ---------------------------------------------------------------------------

-- | Wrap a Kleisli arrow with a name, timing each call and recording in a Map.
meterNamed :: (Ord k) => k -> Kleisli IO a b -> Kleisli IO (Map k [Nanos], a) (Map k [Nanos], b)
meterNamed name k = Kleisli $ \(m, a) -> do
  (t, b) <- runKleisli (meterK timeM k) a
  pure (Map.insertWith (++) name [t] m, b)

-- | Line-by-line pipeline with per-stage timing accumulated across all lines.
--
-- Returns @(measurement map, word counts)@.
wordCountLineByLineMetered :: IO (Map String [Nanos], Map String Int)
wordCountLineByLineMetered =
  withFile "other/alice.md" ReadMode $ \h ->
    loop h Map.empty Map.empty
  where
    loop h acc m = do
      (m1, eof) <- runKleisli (meterNamed "hIsEOF" (Kleisli hIsEOF)) (m, h)
      if eof then pure (m1, acc) else do
        (m2, line) <- runKleisli lineMetered (m1, h)
        (m3, ws) <- runKleisli wordsMetered (m2, line)
        let acc' = ws |> foldl' (\a w -> a |> Map.insertWith (+) w 1) acc
        loop h acc' m3

    lineMetered = meterNamed "hGetLine" (Kleisli hGetLine)
    wordsMetered = meterNamed "getWords" (Kleisli (pure . getWords))
