-- | Producer-consumer: open, read, close as explicit Circuit stages.
{-# LANGUAGE GADTs #-}

module Main where

import Circuit
import Circuit.Circuit (Circuit(..), reify)
import Control.Arrow (Kleisli(..), runKleisli)
import Control.Category ((>>>))
import Data.Bool (bool)
import System.IO (Handle, IOMode(ReadMode), hClose, hGetLine, hIsEOF, openFile)

-- | Domain primitives #################################################

openf :: Circuit (Kleisli IO) t FilePath Handle
openf = Lift (Kleisli (\fp -> openFile fp ReadMode))

getLine :: Circuit (Kleisli IO) t Handle String
getLine = Lift (Kleisli hGetLine)

closef :: Circuit (Kleisli IO) t Handle ()
closef = Lift (Kleisli hClose)

-- | Iteration via Either feedback ####################################

readAll :: Circuit (Kleisli IO) Either Handle (Handle, [String])
readAll = Knot (Kleisli step)
  where
    step (Left (h, acc)) =
      hIsEOF h >>= bool
        (hGetLine h >>= \line -> pure (Left (h, line : acc)))
        (pure (Right (h, acc)))
    step (Right h) =
      pure (Left (h, []))

-- | Pipeline: open → read → close → display ##########################

pipeline :: Circuit (Kleisli IO) Either FilePath ()
pipeline = openf
  >>> readAll
  >>> Lift (Kleisli (\(h, acc) -> hClose h >> putStrLn ("read " <> show (length acc) <> " lines")))

main :: IO ()
main = runKleisli (reify pipeline) "other/alice.md"
