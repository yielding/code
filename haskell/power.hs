power n m = go n m 1
  where
    go n m res 
      | m > 0     = go n (m - 1) (res * n)
      | otherwise = res

