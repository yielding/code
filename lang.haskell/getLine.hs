module ScreenIo where

import System.IO
import Char


getLine :: IO String
getLine = do x <- getChar
            if x == '\n' then
              return []
            else
              do xs <- getLine
                return (x:xs)

main = do getLine