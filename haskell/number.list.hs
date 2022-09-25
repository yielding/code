numberList :: (Num a, Enum a) => String -> [(a, Char)]
numberList xs = zip [0..] xs

main = do print $ numberList "abcd"