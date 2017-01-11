firsts     :: [(a, b)] -> [a]
firsts ps  = [x | (x, _) <- ps]

length'    :: [a] -> Int
length' xs = sum [1 | _ <- xs]

factors    :: Int -> [Int]
factors n  = [x | x <- [1..n], n `mod` x == 0]

prime      :: Int -> Bool
prime n    = factors n == [1, n]

primes     :: Int -> [Int]
primes n   = [x | x <- [2..n], prime x]

find       :: Eq a => a -> [(a, b)] -> [b]
find k t   = [v | (k', v) <- t, k==k']

pairs      :: [a] => [(a, a)]
pairs xs   = [(a, b) | zip(xs, tail xs)]
