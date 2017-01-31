import Data.Char

firsts      :: [(a, b)] -> [a]
firsts ps   = [x | (x, _) <- ps]

length'     :: [a] -> Int
length' xs  = sum [1 | _ <- xs]

factors     :: Int -> [Int]
factors n   = [x | x <- [1..n], n `mod` x == 0]

prime       :: Int -> Bool
prime n     = factors n == [1, n]

primes      :: Int -> [Int]
primes n    = [x | x <- [2..n], prime x]

find        :: Eq a => a -> [(a, b)] -> [b]
find k t    = [v | (k', v) <- t, k==k']

pairs       :: [a] -> [(a, a)]
pairs xs    = zip xs (tail xs)

sorted      :: Ord a => [a] -> Bool
sorted xs   = and [x <= y | (x, y) <- pairs xs]

positions   :: Eq a => a -> [a] -> [Int]
positions x xs
            = [i | (x', i) <- zip xs [0..n], x == x']
              where n = length xs - 1

let2int     :: Char -> Int
let2int c   = ord c - ord 'a'

int2let     :: Int -> Char
int2let n   = chr (ord 'a' + n)

shift       :: Int -> Char -> Char
shift n c   | isLower  c = int2let((let2int c + n) `mod` 26)
            | otherwise  = c

encode      :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


doubleList  :: [Integer] -> [Integer]
doubleList [] = []
doubleList (n:ns) = (2 * n) : doubleList ns


tripleList  :: [Int] -> [Int]
tripleList xs = [3*x | x <- xs]


takeInt     :: Int -> [a] -> [a]
takeInt 0 (x:xs) = []
takeInt n (x:xs)
             = x : takeInt (n-1) xs

dropInt     :: Int -> [a] -> [a]
dropInt _ []     = []
dropInt 0 (x:xs) = (x:xs)
dropInt 1 (x:xs) = xs
dropInt n (x:xs) = dropInt (n-1) xs

applyToInts :: (Int -> Int) -> [Int] -> [Int]
applyToInts _ [] = []
applyToInts f (n:ns) = (f n) : applyToInts f ns

heads :: [[a]] -> [a]
heads = map head
                               
f x = 
    case x of
      0 -> 18
      1 -> 15
      2 -> 12
      _ -> 12 - x

