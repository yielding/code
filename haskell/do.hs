main = do
  putStrLn "The base?"
  w <- getLine
  let width = read w :: Integer
  putStrLn "The height?"
  h <- getLine
  let height = read h :: Integer
  let a = fromIntegral(width * height) / 2
  let area = show a 

  putStrLn("The area of that triangle is " ++ (area))
