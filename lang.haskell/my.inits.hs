myInits :: [a] -> [[a]]
myInits = map reverse . scanl (flip (:)) []

main :: IO ()
main = 
  print $ myInits [1..5]