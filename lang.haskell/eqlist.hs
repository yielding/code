eqList :: Eq a => [a] -> [a] -> Bool
eqList []     []     = True
eqList (x:xs) (y:ys) = x == y && eqList xs ys
eqList  _     _      = False

instance Eq a => Eq [a] where
  (==) = eqList
 
