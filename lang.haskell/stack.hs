--
-- Shows us an ADT
-- only interfaces are fixed and implementations are not complete, open to the actual user.
--
module Stack (Stack, push, pop, top, empty) where

data Stack a = EmptyStk | Stk a (Stack a) 
  deriving (Show)

push x s  = Stk x s
pop (Stk _ s) = s
top (Stk x _) = x
empty = EmptyStk

-- module Stack ( Stack, push, pop, top, empty ) where
-- 
-- newtype Stack a = StackImpl [a] deriving (Show)
-- 
-- push x (StackImpl xs)  = StackImpl (x:xs)
-- pop (StackImpl (_:xs)) = StackImpl xs
-- top (StackImpl (x:_))  = x
-- empty                  = StackImpl []

main = do
  let myStk = push 3 . push 4 . push 2 $ empty
  print(myStk)
  print $ top myStk
  print $ (top.pop) myStk