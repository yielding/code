module Main where

import Control.Monad
import System.Random

main :: IO ()
main = do
  x <- replicateM 5 getLine
  print x

