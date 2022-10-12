find :: Eq a => a -> [(a, b)] -> [b]
find key tuples = [v | (k, v) <- tuples, key == k]

main = do
  let input  = [('a', 1), ('b', 2), ('c', 3), ('b', 5)]
  let result = find 'b' input
  print result
