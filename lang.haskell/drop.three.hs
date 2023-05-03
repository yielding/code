dropThree :: [a] -> [a]
dropThree (_:_:_:xs) = xs
dropThree _ = []

