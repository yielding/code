import Data.Char (toUpper)

-- 3. 함수 합성(.)
-- 함수 합성 연산자(.)는 오른쪽 함수의 결과를 왼쪽 함수의 입력으로 전달합니다. 
-- 즉, map reverse . scanl (flip (:)) []는 scanl (flip (:)) []의 결과를 
-- map reverse로 처리하는 함수입니다.

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