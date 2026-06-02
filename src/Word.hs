module Word where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

wordCount :: [String] -> Map String Int
wordCount = Map.fromListWith (+) . map (,1)
