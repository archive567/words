-- | Word counting as a circuits laboratory.
--
-- R&D journal. Two word-counting pipelines, two resource strategies:
-- all-at-once (getContents) and line-by-line (getLine).
module Words
  ( -- * All-at-once pipeline
    wordCountAllAtOnce,
    wordCountAllAtOnceFile,

    -- * Line-by-line pipeline
    wordCountLineByLine,
    wordCountLineByLineFile,

    -- * Shared
    normalise,
    countWords,
    mergeCounts,
    topN,
    printTopN,
    printFrequencies,
  )
where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import System.IO (Handle, hGetLine, hIsEOF, withFile, IOMode (ReadMode))

-- ---------------------------------------------------------------------------
-- Shared pure functions
-- ---------------------------------------------------------------------------

-- | Normalise a word: keep only a-z, lowercase.
normalise :: String -> String
normalise = map toLower . filter (`elem` ['a' .. 'z'])

-- | Extract normalised words from a chunk of text, dropping empties.
wordsIn :: String -> [String]
wordsIn = filter (not . null) . map normalise . words

-- | Count words in a single chunk.
countWords :: String -> Map String Int
countWords = foldl (\m w -> Map.insertWith (+) w 1 m) Map.empty . wordsIn

-- | Merge two count maps.
mergeCounts :: Map String Int -> Map String Int -> Map String Int
mergeCounts = Map.unionWith (+)

-- | Take the top n words by frequency.
topN :: Int -> Map String Int -> [(String, Int)]
topN n = take n . sortOn (Down . snd) . Map.toList

-- | Print word frequencies, one per line.
printFrequencies :: [(String, Int)] -> IO ()
printFrequencies = mapM_ (\(w, c) -> putStrLn (w ++ ": " ++ show c))

-- | Count words in a string and print the top n.
printTopN :: Int -> String -> IO ()
printTopN n = printFrequencies . topN n . countWords

-- ---------------------------------------------------------------------------
-- Pipeline A: all-at-once (getContents)
-- ---------------------------------------------------------------------------

-- | Read entire contents as a String, count words, print top n.
--
-- @
-- wordCountAllAtOnce n = readFile path >>= printTopN n
-- @
wordCountAllAtOnce :: Int -> FilePath -> IO ()
wordCountAllAtOnce n path = do
  contents <- readFile path
  printTopN n contents

-- | All-at-once pipeline reading from @other\/alice.md@, top 5.
wordCountAllAtOnceFile :: IO ()
wordCountAllAtOnceFile = wordCountAllAtOnce 5 "other/alice.md"

-- ---------------------------------------------------------------------------
-- Pipeline B: line-by-line (getLine)
-- ---------------------------------------------------------------------------

-- | Read a file line by line, accumulating word counts.
-- Mimics a resource-constrained streaming pipeline.
processLineByLine :: Handle -> Map String Int -> IO (Map String Int)
processLineByLine h acc = do
  eof <- hIsEOF h
  if eof
    then pure acc
    else do
      line <- hGetLine h
      let chunkCounts = countWords line
      processLineByLine h (mergeCounts acc chunkCounts)

-- | Read a file one line at a time, count words, print top n.
wordCountLineByLine :: Int -> FilePath -> IO ()
wordCountLineByLine n path = do
  counts <- withFile path ReadMode (`processLineByLine` Map.empty)
  printFrequencies (topN n counts)

-- | Line-by-line pipeline reading from @other\/alice.md@, top 5.
wordCountLineByLineFile :: IO ()
wordCountLineByLineFile = wordCountLineByLine 5 "other/alice.md"
