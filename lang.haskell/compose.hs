import Data.Char (toUpper)

toUpperStr :: String -> String
toUpperStr = map toUpper

addExclamation :: String -> String
addExclamation s = s ++ "!"

shout :: String -> String
shout = addExclamation . toUpperStr


f :: Int -> Int
f x = x * 2

g :: Int -> Int
g x = x + 3

h :: Int -> Int
h = f . g

main = print $ (f . g) 5