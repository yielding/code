cdf1 :: (Num a) => [a] -> [a]
cdf1 = (scanl (+) 0) . ((:) 0)

cdf2 :: (Num a) => [a] -> [a]
cdf2 = drop 2 . (scanl (+) 0) . ((:) 0)

main :: IO()
main = do
  print $ cdf1 [1, 2, 3, 4, 5]
  print $ cdf2 [1, 2, 3, 4, 5]