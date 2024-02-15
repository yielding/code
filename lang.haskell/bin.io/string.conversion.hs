#! /usr/bin/env stack
-- stack --resolver lts-18.8 script

{-# LANGUAGE OverloadedStrings #-}

{-
This is a handy illustration of converting between five of the commonly-used
string types in Haskell (String, ByteString, lazy ByteString, Text and lazy
Text).

Some things to note:

- We are converting between String and ByteString through Text modules
which handles Unicode properly. It's a common (but wrong) practice to instead
use Data.ByteString.Char8 for these conversions, don't do that!

- On that note, it's possible to use Data.ByteString.UTF8 from the utf8-string
package for these conversions using UTF8.toString and UTF8.fromString instead
of Text's (encodeUtf8 . pack) and (unpack . decodeUtf8), but will require that
additional library

- It's possible you need something other than UTF-8. There are more
decode/encode options (like decodeUtf32LE and friends) in Data.Text.Encoding
-}

import Data.ByteString.Char8      as B
import Data.ByteString.Lazy.Char8 as BL
import Data.Text                  as T
import Data.Text.Encoding         as T
import Data.Text.IO               as T
import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL
import Prelude                    as P


main :: IO ()
main = do
  P.putStrLn "from String"
  B.putStrLn  $ T.encodeUtf8 . T.pack                 $ "String to strict ByteString"
  BL.putStrLn $ TL.encodeUtf8 . TL.pack               $ "String to lazy ByteString"
  T.putStrLn  $ T.pack                                  "String to strict Text"
  TL.putStrLn $ TL.pack                                 "String to lazy Text"

  P.putStrLn "\nfrom strict ByteString"
  P.putStrLn  $ T.unpack . T.decodeUtf8               $ "strict ByteString to String"
  BL.putStrLn $ BL.fromChunks . return                $ "strict ByteString to lazy ByteString"
  T.putStrLn  $ T.decodeUtf8                            "strict ByteString to strict Text"
  TL.putStrLn $ TL.fromStrict . T.decodeUtf8          $ "strict ByteString to lazy Text"

  P.putStrLn "\nfrom lazy ByteString"
  P.putStrLn  $ TL.unpack . TL.decodeUtf8             $ "lazy ByteString to String"
  B.putStrLn  $ B.concat . BL.toChunks                $ "lazy ByteString to strict ByteString"
  T.putStrLn  $ T.decodeUtf8 . B.concat . BL.toChunks $ "lazy ByteString to strict Text"
  TL.putStrLn $ TL.decodeUtf8                           "lazy ByteString to lazy Text"

  P.putStrLn "\nfrom strict Text"
  P.putStrLn  $ T.unpack                                "strict Text to String"
  B.putStrLn  $ T.encodeUtf8                            "strict Text to strict ByteString"
  BL.putStrLn $ BL.fromChunks . return . T.encodeUtf8 $ "strict Text to lazy ByteString"
  TL.putStrLn $ TL.fromStrict                           "strict Text to lazy Text"

  P.putStrLn "\nfrom lazy Text"
  P.putStrLn  $ TL.unpack                               "lazy Text to String"
  B.putStrLn  $ T.encodeUtf8 . TL.toStrict            $ "lazy Text to strict ByteString"
  BL.putStrLn $ TL.encodeUtf8                           "lazy Text to lazy ByteString"
  T.putStrLn  $ TL.toStrict                             "lazy Text to strict Text"