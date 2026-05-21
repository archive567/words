-- | Word counting as a circuits laboratory.
--
-- R&D journal. Left-to-right process style.
-- Data flows forward: @h |> readLine |> tokenise |> count@.
module Words
  ( wordCountAllAtOnceFile,
    wordCountLineByLineFile,
    countWords,
    getWords,
    formatTop,

    -- * Process combinators
    (|>),
    (.>),
  )
where

import Control.Arrow ((>>>))
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Function ((&))
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import System.IO (hGetLine, hIsEOF, withFile, IOMode (ReadMode))

-- | Forward application: @x |> f = f x@.
infixl 1 |>
(|>) :: a -> (a -> b) -> b
(|>) = (&)

-- | Forward composition: @f .> g = g . f@.
--
-- >>> (words .> map length) "hello world"
-- [5,5]
infixr 1 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = (>>>)

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
          h
            |> hGetLine
            |> fmap ( getWords
                        .> foldl'
                          (\m w -> m |> Map.insertWith (+) w 1)
                          acc
                    )
            >>= loop h
