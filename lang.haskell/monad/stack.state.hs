import Control.Monad.State

type Stack = [Int]

empty :: Stack
empty = []

pop :: State Stack Int
pop = state $ \ (x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \ xs -> ((),a:xs)

top :: State Stack Int
top = state $ \ (x:xs) -> (x, x:xs)

stackManip :: State Stack Int
stackManip = do -- do는 IO가 아니라 monad를 위한 syntatic sugar
  push 10
  push 20
  a <- pop
  b <- pop
  push (a + b)
  top

main :: IO ()
main = do
  -- 아래 마지막에 state의 기본 state인 empty가 주어지는 것에 주목
  let res = evalState stackManip empty 
  print res
