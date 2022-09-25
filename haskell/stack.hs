module Stack ( StkType, push, pop, top, empty ) where

data StkType a = EmptyStk | Stk a (StkType a) 
  deriving (Show)

push x s  = Stk x s
pop (Stk _ s) = s
top (Stk x _) = x
empty = EmptyStk

-- module Stack ( StkType, push, pop, top, empty ) where
-- 
-- newtype StkType a = Stk [a] deriving (Show)
-- 
-- push x (Stk xs)   = Stk (x:xs)
-- pop (Stk (_:xs))  = Stk xs
-- top (Stk (x:_))   = x
-- empty             = Stk []

main = do
  let myStk = push 3 . push 4 . push 2 $ empty
  print(myStk)
  print $ top myStk
  print $ (top.pop) myStk