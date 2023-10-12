--
-- type class와 concept, 대규모 제품 개발에 있어서 중요한 요소
-- interface가 type class와 유사점은 실제 instance가 있어야 한다는 것
--
data Foo
  = Foo { x :: Integer, str :: String }

instance Eq Foo where
  (Foo x1 str1) == (Foo x2 str2) = (x1 == x2) && (str1 == str2)

data Foo2 = Foo2 { xx :: Integer, ss :: String }
  deriving (Eq, Ord, Show) -- 자동 instanciation이 항상 보장이 되지는 않을 듯

class (Eq a) => Ord2 a where
  comapre              :: a -> a -> Ordereing
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min             :: a -> a -> a

class (Num a, Ord a) => Real a where
  --
  toRational           :: a -> Rational

f1 = Foo 3 "orange"
f2 = Foo 4 "apple"


foo :: (Num a, Show a, Show b) => a -> a -> b -> String
foo x y t =
  show x ++ " + " show y ++ " = " ++ show (x+y) ++ ", " ++ show t

main = do 
  print $ f1 == f2
  print $ f1 /= f2