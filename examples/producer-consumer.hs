{-# LANGUAGE GADTs #-}

-- | Producer-consumer: open, read, close as explicit Trace stages.
module Main where

import Circuit
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Category ((>>>))
import Data.Bool (bool)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetLine, hIsEOF, openFile)

-- | Domain primitives #################################################
openf :: Trace t (Kleisli IO) FilePath Handle
openf = Arr (Kleisli (`openFile` ReadMode))

getLine :: Trace t (Kleisli IO) Handle String
getLine = Arr (Kleisli hGetLine)

closef :: Trace t (Kleisli IO) Handle ()
closef = Arr (Kleisli hClose)

-- | Iteration via Either feedback ####################################
readAll :: Trace Either (Kleisli IO) Handle (Handle, [String])
readAll = Knot (Kleisli step)
  where
    step (Left (h, acc)) =
      hIsEOF h
        >>= bool
          (hGetLine h >>= \line -> pure (Left (h, line : acc)))
          (pure (Right (h, acc)))
    step (Right h) =
      pure (Left (h, []))

-- | Pipeline: open → read → close → display ##########################
pipeline :: Trace Either (Kleisli IO) FilePath ()
pipeline =
  openf
    >>> readAll
    >>> Arr (Kleisli (\(h, acc) -> hClose h >> putStrLn ("read " <> show (length acc) <> " lines")))

main :: IO ()
main = runKleisli (run pipeline) "other/alice.md"
