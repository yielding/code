module Main where

import Data.Char (toUpper)
import Control.Monad

-- liftM :: Monad m => (a1 -> m) -> m a1 -> m r
-- 즉 주어진 함수에서 비모나딕 결과를 취해 단지 (>>=)를 위해 모나딕 값을 반환
--
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- 즉, 비모나딕 값을 취하는 함수(a -> m b)에 모나딕 값(m a)을 전달해서 
-- 모나딕 값을 돌려 받는 함수
main1 = putStrLn "Write your string: " >> liftM shout getLine >>= putStrLn
main  = do putStrLn "Write your string: " 
           string <- getLine 
           putStrLn (shout string)

-- 주어진 문자열을 대문자 문자열로 바꾸는 함수
shout = map toUpper
