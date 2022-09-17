positions :: Eq a => a -> [a] -> [Int]  
positions pos xs = [i | (x, i) <- zip xs [0..n], pos == x]
  where
    n = length xs - 1

main = do
  let res = positions 2 in1
  print(res)
