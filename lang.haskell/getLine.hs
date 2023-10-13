module ScreenIo where

import System.IO

getLine2 :: IO String
getLine2 = do 
  x <- getChar
  if x == '\n' then return []
               else do xs <- getLine2
                       return (x:xs)

main = do 
  s <- getLine2
  print s 