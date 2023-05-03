reverse1 :: [a] -> [a]
reverse1 []     = []
reverse1 (x:xs) = reverse1 xs ++ [x]

reverse2 :: [a] -> [a]
reverse2 xs = foldr snoc [] xs
  where
    snoc x xs = xs ++ [x]

main = do
  print $ reverse2 [1..10]