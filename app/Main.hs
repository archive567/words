module Main where

import Circuit
import Circuit.Meter.Time (meterIO)
import Control.Arrow (Kleisli (..), runKleisli, (>>>))
import Control.DeepSeq (force)
import Control.Monad ((>=>))
import Data.Bool (bool)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.IO (Handle, IOMode (ReadMode), hGetLine, hIsEOF, withFile)
import Word (formatTop, getWords)

foldWords :: Map String Int -> [String] -> Map String Int
foldWords =
  foldl' (\m w -> Map.insertWith (+) (force w) (1 :: Int) m)

countFile :: IO ()
countFile =
  withFile "other/alice.md" ReadMode countLines

countLines :: Handle -> IO ()
countLines h =
  loop h Map.empty >>= (formatTop 5 >>> putStr)

loop :: Handle -> Map String Int -> IO (Map String Int)
loop h acc =
  hIsEOF h >>= bool (step h acc) (pure acc)

step :: Handle -> Map String Int -> IO (Map String Int)
step h acc =
  h & (hGetLine >=> pure . (getWords >>> foldWords acc) >=> loop h)

perfTest :: IO ()
perfTest = do
  (t, ()) <- runKleisli (reify (meterIO (const countFile))) ()
  let ms = fromIntegral t / 1_000_000 :: Double
  putStrLn $ " wall: " ++ show ms ++ " ms"

main :: IO ()
main = perfTest
