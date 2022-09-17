sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]
  where 
    pairs :: [a] -> [(a, a)]
    pairs xs = zip xs (tail xs)

main = do
  let in1 = [1, 2, 3, 2]
  print(sorted in1)