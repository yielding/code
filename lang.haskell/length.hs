length1 :: [a] -> Int
length1 [] = 0
length1 (_:xs) = 1 + length1 xs

length2 :: [a] -> Int
length2 xs 
  = foldr (\_ n -> n+1) 0 xs

main = do 
  print(length2 [1..10])
