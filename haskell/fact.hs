factorial n = go n 1
  where
    go n res
      | n > 1     = go (n - 1) (res * n)
      | otherwise = res

main = do
  print $ factorial 100
