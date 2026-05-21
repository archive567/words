-- | Word counting as a circuits laboratory.
--
-- R&D journal. Two word-counting pipelines, two resource strategies:
-- all-at-once (getContents) and line-by-line (getLine).
module Words
  ( wordCountAllAtOnceFile,
    wordCountLineByLineFile,
  )
where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import System.IO (Handle, hGetLine, hIsEOF, withFile, IOMode (ReadMode))

-- $setup
-- >>> import Words

-- | All-at-once pipeline: read entire file, count words, print top 5.
--
-- >>> wordCountAllAtOnceFile
-- the: 1523
-- and: 779
-- to: 720
-- a: 616
-- she: 501
wordCountAllAtOnceFile :: IO ()
wordCountAllAtOnceFile = undefined

-- | Line-by-line pipeline: read one line at a time, accumulate counts, print top 5.
-- Mimics a resource-constrained streaming pipeline.
--
-- >>> wordCountLineByLineFile
-- the: 1523
-- and: 779
-- to: 720
-- a: 616
-- she: 501
wordCountLineByLineFile :: IO ()
wordCountLineByLineFile = undefined
