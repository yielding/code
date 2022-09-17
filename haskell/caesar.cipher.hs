import Data.Char

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
  where
    shift :: Int -> Char -> Char 
    shift n c 
      | isLower c = int2let ((let2int c + n) `mod` 26)
      | otherwise = c

    let2int :: Char -> Int
    let2int c = ord c - ord 'a'

    int2let :: Int -> Char
    int2let n = chr (ord 'a' + n)

main = do 
  -- putStrLn(encode 1 "hello")
  let f = \i -> encode i "hello"
  mapM_ putStrLn $ map f [0..10]