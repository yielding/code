isEven   :: Int -> Bool 
isEven n = ((mod n 2) == 0)

retainEven = filter isEven

divisors p = map f [1..p]
