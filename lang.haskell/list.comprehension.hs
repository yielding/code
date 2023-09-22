echoes = foldr (\ x xs -> (replicate x x) ++ xs) [] 

main = do
  print $ [(x, y) | x <- [1..4], y <- [5..8], x + y > 8]
  print $ echoes [1, 2, 3]