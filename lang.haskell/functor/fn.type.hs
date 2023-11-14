
{-
   a -> b 
   - a type을 받아 b type을 반환하는 함수
   - (->) a b : 전위 표기법
   - (->) a   : 부분 적용, 타입 인자 하나를 받아서 새로운 타입(함수) 생성하는 
                타입 생성자
   - Applicative Functor, Monad의 인스턴스 생성 가능


instance Functor ((->) r) where
  fmap f g = (\x -> f(g x))

일반적으로 fmap은
fmap :: (a -> b) -> f a -> f b

(->) r 에 대한 fmap
fmap :: (a -> b) -> (->) r a -> (->) r b

위 표현을 중위 표현
fmap :: (a -> b) -> (r -> a) -> (r -> b)

위 식은 바로 합성함수의 타입
즉, 

instance Functor ((->) r) where
  fmap f g = (.)           -- 대박


instance Applicative ((->) r) where
  pure x  = (\_ -> x)
  f <*> g = \x -> f x (g x)

일반적으로 <*>는 
(<*>) :: f (a -> b) -> f a -> f b

f 대신 (->)r 을 넣으면

(<*>) :: (->)r (a -> b) -> (->) r a -> (->) r b

위 표현을 중위 표현으로
(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
f <*> g = \x -> f x (g x)

즉, <*> 함수는 어떤 r 타입의 인자 x를 받아서 함수 f에 x와 g x를 인자로 넘긴 결과를 반환하는 
새로운 함수를 리턴


-}

main :: IO ()
main = do
  print $ fmap (*3) (+100) 1      -- 303
  print $ (*3) `fmap ` (+100) $ 1 -- 303
  print $ (*3) . (+100) $ 1       -- 303

  print $ fmap (show . (*3)) (*100) 1