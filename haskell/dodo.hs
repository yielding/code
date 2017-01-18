promptLine :: String -> IO String
promptLine prompt = do
  putStr prompt
  getLine

main :: IO ()
main = do
  line1 <- promptLine "Enter a line: "
  line2 <- promptLine "Enter another line: "
  putStrLn ("you said: " ++ line1 ++ " and " ++ line2)
