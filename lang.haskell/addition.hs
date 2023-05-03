plusOne x = x + 1

addition x y = go x y x
  where 
    go x y res
      | y > 0     = go x (y-1) (plusOne res)
      | y < 0     = go x (plusOne y) (res - 1)
      | otherwise = res