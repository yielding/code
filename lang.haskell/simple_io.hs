main = do
  putStrLn "The Base?"
  base <- getLine
  putStrLn "The Height?"
  height  <- getLine
  let res = (read base :: Float) * (read height :: Float)
  putStrLn ("The Base is : " ++ show(res))
