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
  setLocation (x, y) p = p {pointX = x, pointY = y }

move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p
  where
    (x, y) = getLocation p

point = NamedPoint { pointName = "pt", pointX = 10, pointY = 10 }

main = do 
  let res = move (10, 10) point
  print res