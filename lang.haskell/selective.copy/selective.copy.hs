{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as Ch8
import System.IO
import System.IO.Error

main :: IO()
main = do
  Ch8.putStrLn "processing.."
  withFile "input.txt" ReadMode $ \ ih ->
    withFile "output.txt" WriteMode $ \ oh ->
      untilIOError (selectiveTransfer ih oh)
  Ch8.putStrLn "done!"

selectiveTransfer :: Handle -> Handle -> IO ()
selectiveTransfer ih oh = do
  line <- Ch8.hGetLine ih
  if "system:" `Ch8.isPrefixOf` line
     then Ch8.hPutStrLn oh line
     else return ()

untilIOError :: IO a -> IO ()
untilIOError action = 
  catchIOError (forever action) (\_ -> return ())