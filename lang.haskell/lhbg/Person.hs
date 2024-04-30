-- import Prelude (hiding Left Right )
-- record repr. is syntatic sugar for data type
data Person
  = Person 
    { name :: String
    , age  :: Int
    }
    deriving (Show)

person = Person { name = "yielding", age = 52 }

data Either a b
  = Left a
  | Right b

a = Main.Left "Hello"
b = Main.Right 19

main :: IO()
main = do 
  print $ age person
  print $ name person
