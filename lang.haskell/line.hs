data Point 
  = Point Float Float
    deriving (Show, Eq)

data Vector 
  = Vector Float Float
    deriving (Show, Eq)

type Line = (Point, Point)

data LineStyle 
  = Solid
  | Dashed
  | Dotted

type FancyLine = (Point, Point, LineStyle)

data Day
  = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Enum, Eq)

isWeekday :: Day -> Bool
isWeekday day 
  = not $ day `elem` [Saturday, Sunday]

zeroPoint :: Point
zeroPoint = Point 0 0

pointToVector :: Point -> Point
pointToVector (Point x y) = Vector x y

main :: IO ()
main = do
  print $ isWeekday Monday