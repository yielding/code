concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x<-xs]

concat[[1, 2, 3], [4, 5], [6]]
