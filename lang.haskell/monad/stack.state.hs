import Control.Monad

newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a
state = State

instance Functor (State s) where
  fmap fn (State sa) = State (\s0 -> let (a, s1) = sa s0 in (fn a, s1))

instance Applicative (State s) where
  pure a = State (\s -> (a, s))
  (State sf) <*> (State sa) =
    State (\s0 -> let (fn,s1) = sf s0
                      (a, s2) = sa s1
                  in (fn a, s2))

instance Monad (State s) where
  return = pure

  p >>= k = state $ \ s0 ->
    let (x, s1) = runState p s0  -- Running the first processor on s0.
                                 -- runState에 state s를 넘기면 
    in runState (k x) s1

  -- 아래 함수와 위 함수가 동치라고 하는 것 같은데 이해가 잘 안됨
  -- State act >>= k = State $ \s ->
  --   let (a, s') = act s
  --   in runState (k a) s'

get :: State s s
get = State $ \ s -> (s, s)

put :: s -> State s ()
put s = State $ \ _ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \ x -> put (f x)

evalState :: State s a -> s -> a
evalState act = fst . runState act

execState :: State s a -> s -> s
execState act = snd . runState act

-- Stack
type Stack = [Int]

empty :: Stack
empty = []

pop :: State Stack Int
pop = State $ \ (x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = State $ \ xs -> ((),a:xs)

tos :: State Stack Int
tos = State $ \ (x:xs) -> (x, x:xs)

stackManip :: State Stack Int
stackManip = do
  push 10
  push 20
  a <- pop
  b <- pop
  push (a+b)
  tos

main :: IO ()
main = do
  let res = evalState stackManip empty
  print res
