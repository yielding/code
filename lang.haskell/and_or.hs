and_ :: [Bool] -> Bool
and_ []     = True
and_ (x:xs) = x && and_ xs

reverse bs = foldr (\b g x -> g (b : x)) id bs []

-- and_1 :: [Bool] -> Bool
-- and_1 = foldr (\x xs -> x && xs) []

or_ :: [Bool] -> Bool
or_ []     = False
or_ (x:xs) = x || or_ xs
