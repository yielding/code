--
-- 책에서는 실제로 type이 잘 모델링 되었는지를 생각하지 말라고 한다. 
-- 문법으로는 이렇게 동작할 수 있다는 이야기..
-- 초보인 나로서는 type class, instantiatoin 등을 공부하기 너무 좋은 예제
--  
class Located a where
  getLocation :: a -> (Int, Int)

class (Located a) => Movable a where
  setLocation :: (Int, Int) -> a -> a

data NamedPoint
  = NamedPoint 
  { pointName :: String
  , pointX    :: Int
  , pointY    :: Int
  }
  deriving (Show)

instance Located NamedPoint where
  getLocation p = (pointX p, pointY p)

instance Movable NamedPoint where
  setLocation (x, y) p = p { pointX = x, pointY = y }

move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p
  where
    (x, y) = getLocation p

point = NamedPoint { pointName = "pt", pointX = 10, pointY = 10 }

main = do 
  let res = move (10, 10) point
  print res