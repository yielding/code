module Main where

import Control.Monad

main :: IO ()
main = do
  x <- replicateM 5 getLine
  print x

