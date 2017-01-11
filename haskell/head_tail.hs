head        :: [a] -> a
head (x:_)  = x
head []     = error "Prelude.head: empty list"

tail        :: [a] -> [a]
tail (_:xs) = xs
tail []     = error "Prelude.tail: empty list"
