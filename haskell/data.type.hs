data Pair a b = Pair a b

xx = Pair 1 2
yy = Pair "Howdy" 42

first :: Pair a b -> a
first (Pair x _) = x

apply :: (a -> a') -> (b -> b') -> Pair a b -> Pair a' b'
apply f g (Pair x y) = Pair (f x) (g y)

---------------------------------------------------------------
--
---------------------------------------------------------------
type Pos = (Int, Int)

origin :: Pos
origin = (0, 0)

---------------------------------------------------------------
--
---------------------------------------------------------------
data Person = Person Name Gender Age
type Name = String
data Gender = Male | Female
type Age = Int

name (Person n _ _) = n

findPerson name (p@(Person n _ _):ps)
  | name ==  n = p
  | otherwise  = findPerson name ps

showPerson :: Person -> String
showPerson p = show(name p) -- ++ show 
  
main = do 
  let x = Person "Jerry" Female 12
  print(name x)
  let y = [Person "Yoda" Male 999, Person "Tom" Male 7]
  print $ showPerson $ findPerson "Yoda" y

  print origin