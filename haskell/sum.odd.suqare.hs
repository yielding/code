sum_odd_square1 [] = 0
sum_odd_square1 (x:xs)
  | odd x     = x * x + sum_odd_square xs
  | otherwise = sum_odd_square xs

sum_odd_square2 [] = 0
sum_odd_square2 xs = foldr (+) 0 $ map square $ filter odd xs
  where 
    square x = x * x

main = do 
  print(sum_odd_square2 [1, 3])
  print(sum_odd_square1 [1, 3])