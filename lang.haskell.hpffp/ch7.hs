tensDigit :: Integral a =>  a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a =>  a -> a
tensDigit' x = d
  where (xLast, _) = x `divMod` 10
        (_, d) = xLast `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b 
  | b = x
  | otherwise = y
  
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (a, b) = f a b

add :: (Int, Int) -> Int
add (x, y) = x + y

add' :: Int -> Int -> Int
add' = curry' add

addAndDrop = const . (1 +)

reverseMkTuple = flip (,)

reverseTuple = uncurry (flip (,))

main :: IO()
main = do
  print $ reverseTuple (2, 1) -- (1,2)
  print $ reverseMkTuple 1 2  -- (2,1)
  print $ addAndDrop 2 5      -- 3
  print $ tensDigit' 234      -- 3
  print $ foldBool 1 2 True   -- 1
  print $ foldBool 1 2 False  -- 2
  print $ g (+5) (1, 2)
  print $ add' 1 2