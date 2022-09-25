
--data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
--instance Show Weekday where
--  show Mon = "Monday"
--  show Tue = "Tuesday"

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
              deriving (Show, Read, Eq, Ord, Bounded, Enum)

main = do
  print $ map show [Mon, Tue]
  print $ Mon < Tue
  print $ Tue < Tue
  print (read "Fri" :: Weekday)
  print (minBound :: Weekday)
  print (maxBound :: Weekday)
  print $ succ Mon
  print $ pred Fri