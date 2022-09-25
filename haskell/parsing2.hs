module Parsing (module Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char

--
-- https://stackoverflow.com/questions/31815310/the-difference-between-type-and-newtype-in-haskell
-- P :: (P :: (String -> [(a, String)]) -> Parser a
newtype Parser a = P(String -> [(a, String)])

main = do putStrLn("hi")