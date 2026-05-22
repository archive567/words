-- | Word counting as a circuits laboratory.
--
-- Plain ordinary pipeline: left-to-right, no metering, no circuits.
module Words
  ( -- * Pure stages
    getWords,
    countWords,
    formatTop,

    -- * IO pipelines
    wordCountAllAtOnceFile,
    wordCountLineByLineFile,
    wordCountLineByLine,

    -- * Combinators
    (|>),
    (.>),
    (>->),
  )
where

import Control.Category ((>>>))
import Control.DeepSeq (force)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import System.IO (Handle, IOMode (ReadMode), hGetLine, hIsEOF, withFile)

-- | Forward application: @x |> f = f x@.
infixl 1 |>

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

-- | Forward composition: @f .> g = g . f@.
infixr 1 .>

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = (>>>)

-- | Forward Kleisli composition: @f >-> g = \\x -> f x >>= g@.
infixr 1 >->

(>->) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
f >-> g = \x -> f x >>= g

-- ---------------------------------------------------------------------------
-- Pure stages
-- ---------------------------------------------------------------------------

-- | Extract words from a string: split, keep a-z, lowercase, drop empties.
--
-- >>> "Hello, World!" |> getWords
-- ["hello","world"]
getWords :: String -> [String]
getWords =
  words
    .> map (map toLower . filter (`elem` ['a' .. 'z']))
    .> filter (not . null)

-- | Count word frequencies from a string.
--
-- >>> "a b a" |> countWords |> Map.toList
-- [("a",2),("b",1)]
countWords :: String -> Map String Int
countWords = getWords .> map (\w -> (w, 1)) .> Map.fromListWith (+)

-- | Format the top N word frequencies.
--
-- >>> let m = Map.fromList [("the", 10), ("a", 5), ("cat", 3)]
-- >>> putStr $ formatTop 2 m
-- the: 10
-- a: 5
formatTop :: Int -> Map String Int -> String
formatTop n =
  Map.toList
    .> sortOn (Down . snd)
    .> take n
    .> map fmt
    .> unlines
  where
    fmt (w, c) = w ++ ": " ++ show c

-- ---------------------------------------------------------------------------
-- All-at-once pipeline
-- ---------------------------------------------------------------------------

-- | Read entire file, count words, print top 5.
--
-- >>> wordCountAllAtOnceFile
-- the: 1523
-- and: 779
-- to: 720
-- a: 616
-- she: 501
wordCountAllAtOnceFile :: IO ()
wordCountAllAtOnceFile =
  readFile "other/alice.md"
    >>= putStr . (countWords .> formatTop 5)

-- ---------------------------------------------------------------------------
-- Line-by-line pipeline (resource-constrained)
-- ---------------------------------------------------------------------------

-- | Read one line at a time, accumulate, print top 5.
wordCountLineByLineFile :: IO ()
wordCountLineByLineFile =
  withFile "other/alice.md" ReadMode wordCountLineByLine

-- | Line-by-line word counting on an open Handle.
--
-- The inner loop reads a line, counts words, and recurses with the
-- updated accumulator.  No intermediate list of lines is materialised.
wordCountLineByLine :: Handle -> IO ()
wordCountLineByLine h =
  loop Map.empty >>= (formatTop 5 .> putStr)
  where
    loop :: Map String Int -> IO (Map String Int)
    loop acc =
      hIsEOF h >>= bool step (pure acc)
      where
        step :: IO (Map String Int)
        step =
          h
            |> ( hGetLine
                   >-> pure
                   . (getWords .> foldWords acc)
                   >-> loop
               )

    foldWords :: Map String Int -> [String] -> Map String Int
    foldWords acc ws =
      foldl' (\m w -> Map.insertWith (+) (force w) (1 :: Int) m) acc ws
