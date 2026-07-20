{-# LANGUAGE GADTs #-}

-- | Producer-consumer: open, read, close as explicit Loop stages.
module Main where

import Circuit
import Circuit.Category ((.>))
import Control.Arrow (Kleisli (..), runKleisli)
import Data.Bool (bool)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetLine, hIsEOF, openFile)

-- | Domain primitives #################################################
openf :: Loop t (Kleisli IO) FilePath Handle
openf = Lift (Kleisli (`openFile` ReadMode))

getLine :: Loop t (Kleisli IO) Handle String
getLine = Lift (Kleisli hGetLine)

closef :: Loop t (Kleisli IO) Handle ()
closef = Lift (Kleisli hClose)

-- | Iteration via Either feedback ####################################
readAll :: Loop Either (Kleisli IO) Handle (Handle, [String])
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
pipeline :: Loop Either (Kleisli IO) FilePath ()
pipeline =
  openf
    .> readAll
    .> Lift (Kleisli (\(h, acc) -> hClose h >> putStrLn ("read " <> show (length acc) <> " lines")))

main :: IO ()
main = runKleisli (run pipeline) "other/alice.md"
