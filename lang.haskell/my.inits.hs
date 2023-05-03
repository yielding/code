myInits :: [a] -> [[a]]
myInits = map reverse . scanl (flip (:)) []