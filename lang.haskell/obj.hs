data Object 
  = Object { a :: Int }

myObj = Object { a = 1 }

main :: IO ()
main = do 
  print (a myObj)