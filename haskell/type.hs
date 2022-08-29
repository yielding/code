data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

data [a]    = [] | (:) a [a]

data List a = Nil | Cons a (List a)


class Eq a where
  (==), (/=) :: a -> a -> Bool

  x /= y = not (x == y)
  x == y = not (x /= y)


data Foo = Foo { x :: Integer, str :: String}

instance Eq Foo where
  (Foo x1 str1) == (Foo x2 str2) = (x1 == x2) && (str1 == str2)

data Foo2 = Foo2 { x :: Integer, str :: String }
  deriving (Eq, Ord, Show)

class (Eq a) => Ord a where
  compare               :: a -> a -> Ordering
  (<), (<=), (>=), (>)  :: a -> a -> Bbool
  max, min              :: a -> a -> a

class (Num a, Ord a) => Real a where
  toRational  :: a -> Rational

(+) :: (Num a) => a -> a -> a

-- 리스트 보충설명에서 mapㅇㅡㄹ 처음 소개할 때, 리스트 원소에 대한 아주 
-- 구체적인 함수에서 시작하여 일반화를 거쳐 map을 모든 종류의 리스트에 
-- 들어맞는 함수와 결합했다.
-- 더 일반화하여, 리스트용 map, 트리용 map 등을 만드는 대신 매핑 가능한 
-- 모든 종류의 타입에 대한 범용 map을 두는 것이 어떨까?
--

class Functor f where
  fmap  :: (a -> b) -> f a -> f b

instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Functor [] where
  fmap = map

instance Functor Tree where 
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

