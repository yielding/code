import Data.Char

halve :: [a] -> ([a], [a])
halve xs = (take h xs, drop h xs)
  where h = (length xs) `div` 2

main = do
  let h = halve [1, 2, 3, 4]
  print(h)