import Data.List

add_str :: String -> Float -> Float
add_str str x = read str + x

sum_str :: [String] -> Float
sum_str = foldr add_str 0.0
