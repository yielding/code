
data Person
  = Person 
    { name :: String
    , age  :: Int
    }
    deriving (Show)

person = Person {  name = "yielding", age = 52 }


main :: IO()
main = print $ age person
