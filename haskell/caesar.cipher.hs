import Data.Char
import Data.ByteString (count)

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1,  6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

--percent :: Int -> Int -> Int
--percent n m = round $ (fromIntegral n / fromIntegral m) * 100
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count' x xs) n | x <- ['a'..'z']]
  where 
    n = lowers xs

    count' :: Char -> String -> Int
    count' ch xs = length [x | x <- xs, ch == x]

positions :: Eq a => a -> [a] -> [Int]  
positions pos xs = [i | (x, i) <- zip xs [0..n], pos == x]
  where
    n = length xs - 1

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum[((o -e)^2)/e | (o, e) <- zip os es]

rotate      :: Int -> [a] -> [a]
rotate n xs =  drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs

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
  -- let f = \i -> encode i "hello"
  -- mapM_ putStrLn $ map f [0..10]
  --print(percent 2 10)
  -- print(count' 'a' "aabb")
  print(crack "kdvnhoo lv ixq")