filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1 f (x:xs) = if f x
                  then x : filter1 f xs
                  else filter1 f xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 p (x:xs) | p x       = x : filter2 p xs
                 | otherwise = filter2 p xs

filter3 :: (a -> Bool) -> [a] -> [a]
filter3 p xs = [x | x <- xs, p x]

main = do
  print(filter1 (>5)[1..10])