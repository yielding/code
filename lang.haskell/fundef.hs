isDigit' :: Char -> Bool
isDigit' c = c >= '0' && c <= '9'

splitAt'      :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

abs'   :: Int -> Int
abs' n = if n >= 0 then n else (negate n)

abs2 n | n >= 0    =  n
       | otherwise = -n

sign_num n | n < 0     = -1
           | n == 0    = 0
           | otherwise = 1
