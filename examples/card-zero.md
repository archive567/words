# card-zero ⟜ line-by-line word count, lifted to top level

Copy code blocks into the repl:

```
cabal repl word
```

## Imports

```haskell
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Category ((>>>))
import Control.DeepSeq (force)
import Control.Monad ((>=>))
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Function ((&))
import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import System.IO (Handle, IOMode (ReadMode), hGetLine, hIsEOF, withFile)

import Circuit
import Circuit.Meter.Time
```

## top-down | left-to-right

We document in order of left-to-right usage.

```haskell
xCountFile :: IO ()
xCountFile =
  withFile "other/alice.md" ReadMode xCount

xCount :: Handle -> IO ()
xCount h =
  xLoop h Map.empty >>= (xFormatTop 5 >>> putStr)

```

Note the mutual recursion between xLoop and xStep

``` haskell
xLoop :: Handle -> Map String Int -> IO (Map String Int)
xLoop h acc =
  hIsEOF h >>= bool (xStep h acc) (pure acc)

xStep :: Handle -> Map String Int -> IO (Map String Int)
xStep h acc =
  h & (hGetLine >=> pure . (xGetWords >>> xFoldWords acc) >=> xLoop h)

```

``` haskell
xGetWords :: String -> [String]
xGetWords =
  words
    >>> map (map toLower . filter (`elem` ['a' .. 'z']))
    >>> filter (not . null)

xFoldWords :: Map String Int -> [String] -> Map String Int
xFoldWords acc ws =
  foldl' (\m w -> Map.insertWith (+) (force w) (1 :: Int) m) acc ws

xFormatTop :: Int -> Map String Int -> String
xFormatTop n =
  Map.toList
    >>> sortOn (Down . snd)
    >>> take n
    >>> map fmt
    >>> unlines
  where
    fmt (w, c) = w ++ ": " ++ show c
```

## Performance test

```haskell
perfTest :: IO ()
perfTest = do
  (t, ()) <- runKleisli (run (meterIO (const xCountFile))) ()
  let ms = fromIntegral t / 1_000_000 :: Double
  putStrLn $ " wall: " ++ show ms ++ " ms"
```

## Verified

- [x] 2921 unique words
- [x] top 5: the (1523), and (779), to (720), a (616), she (501)
- [x] wall time ~62 ms
