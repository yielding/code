isEven :: Int -> Bool
isEven n = ((mod n 2) == 0)

retainEven ns = filter isEven ns
