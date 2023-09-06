import Prelude   hiding(foldr, foldl, foldr1)
import Data.List hiding(foldr, foldl, foldr1)

{- foldr : fold from right to left

  :                         f
 / \                       / \
a   :       foldr f acc   a   f
   / \    ------------->     / \
  b   :                     b   f
     / \                       / \
    c  []                     c   acc

-}

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc []     = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

add_str :: String -> Float -> Float
add_str str x = read str + x

sum_str :: [String] -> Float
sum_str = foldr add_str 0.0

sum' :: [Integer] -> Integer
sum' = foldr (+) 0

{- foldl : fold from left to right

foldl is tail recursion. General compiler can perform tail recursion optimization.
But, due to the layiness of haskell, this language does not perform tail recursion optimization.
So, there is one more eager version of fold called foldl' (fold - L -tick) in Data.List

그래서, 굳이 foldl을 쓸 이유가 잘 없다 (foldr 혹은 foldl' 둘 중하나 사용)


    :                            f
  / \                          / \
  a   :       foldl f acc      f   c
    / \    ------------->    / \
    b   :                    f   b 
      / \                  / \
      c  []                acc a


-}

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

{-
   만일 리스트원소와 결과의 타입이 같은 경우는 foldr1, foldl1 을 사용할 수 있다.
-}

addStr :: String -> Float -> Float
addStr str x = read str + x

sumStr :: [String] ->  Float
sumStr = foldr addStr 0.0

foldr1           :: (a -> a -> a) -> [a] -> a
foldr1 f [x]     =  x
foldr1 f (x:xs)  =  f x (foldr1 f xs)
foldr1 _ []      =  error "Prelude.foldr1: empty list"

main = do
  -- foldr
  print $ sum_str ["1", "2"]
  print $ sum' [1..10]