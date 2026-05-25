-- | Word counting as a circuits laboratory.
module Word
  ( -- * Pure stages
    wordCount,
    getWords,
    countWords,
    formatTop,
  )
where

import Control.Category ((>>>))
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))

-- | Count word frequencies from a list of words.
--
-- >>> wordCount ["hello", "world", "hello"]
-- fromList [("hello",2),("world",1)]
wordCount :: [String] -> Map String Int
wordCount = Map.fromListWith (+) . map (,1)

-- | Extract words from a string: split, keep a-z, lowercase, drop empties.
--
-- >>> getWords "Hello, World!"
-- ["hello","world"]
getWords :: String -> [String]
getWords =
  filter (not . null)
    . map (filter (`elem` ['a' .. 'z']) . map toLower)
    . words

-- | Count word frequencies from a string.
--
-- >>> countWords "a b a"
-- fromList [("a",2),("b",1)]
countWords :: String -> Map String Int
countWords = getWords >>> map (,1) >>> Map.fromListWith (+)

-- | Format the top N word frequencies.
--
-- >>> putStr $ formatTop 2 (Data.Map.Strict.fromList [("the", 10), ("a", 5), ("cat", 3)])
-- the: 10
-- a: 5
formatTop :: Int -> Map String Int -> String
formatTop n =
  Map.toList
    >>> sortOn (Down . snd)
    >>> take n
    >>> map fmt
    >>> unlines
  where
    fmt (w, c) = w <> ": " <> show c
