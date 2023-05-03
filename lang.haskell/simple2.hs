main :: IO ()
main = do
  line <- getLine
  putStrLn ("You said: " ++ line)
