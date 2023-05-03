replicate2 :: Int -> a -> [a]
replicate2 n ch = go n []
  where
    go n res 
      | n > 0 = go (n-1) (ch:res)
      | otherwise = res
