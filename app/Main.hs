module Main where

import           Runner
import           System.Environment

main :: IO ()
main = do
  (directory:_) <- getArgs
  run directory
