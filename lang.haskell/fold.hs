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

- 우결합성 접기가 하스켈에서 좌결합성 접기보다 자연스러운 이유는 
  오른쪽 접기는 무한 리스트에 대해 기능할 수 있기 때문이다.

-}

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc []     = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

sum' :: [Integer] -> Integer
sum' = foldr (+) 0

{- foldl : fold from left to right

- foldl is tail recursion. 
  x:xs에서 
  a) acc' = f x acc 
  b) xs가 empty가 될때까지 반복. 

- General compiler can perform tail recursion optimization.
  But, due to the layiness of haskell, 
  this language does not perform tail recursion optimization.
  그래서, 굳이 foldl을 쓸 이유가 잘 없다 

  한 가지 더, foldl은 일단 terminal을 만날때까지 계속해서 재귀를 돌아야하는데
  무한 리스트의 경우는 끝가지 가지전에 무한 재귀로 인한 stackOverflow를 만난다.


- So, there is one more eager version of fold called foldl' (fold - L -tick) 
  in Data.List module

  (foldr 혹은 foldl' 둘 중하나 사용)


   :                            f
  / \                          / \
  a  :       foldl f acc      f   c
    / \    ------------->    / \
    b  :                    f   b 
      / \                  / \
      c  []               acc a

-}

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

{-
   만일 리스트원소와 결과의 타입이 같은 경우는 foldr1, foldl1 을 사용할 수 있다.
   - foldr1
     empty list가 없다는 가정하에 마지막 남은 원소가 초기값.
  
   - foldl1
-}

addStr :: String -> Float -> Float
addStr str x = read str + x

sumStr :: [String] ->  Float
sumStr xs = foldr addStr 0.0 xs

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x]    =  x                        
foldr1 f (x:xs) =  f x (foldr1 f xs)
foldr1 _ []     =  error "Prelude.foldr1: empty list"


foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl1 (f x) xs
foldl1 _ []     = error "Prelude.foldl1: empty list"

-- sum' :: [Integer] -> Integer
-- sum' = foldr (+) 0
sum2 :: [Integer] -> Integer
sum2 = foldr1 (+)

main = do
  -- foldr
  print $ sumStr ["1", "2"]
  print $ sum' [1..10]
  print $ sum2 [1..10]
