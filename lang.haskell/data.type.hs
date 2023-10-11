-- 열거형
-- 1. data 선언의 특수 형태
-- 2. 생성자가 인자가 없는 형테
data Month = January | February | March | December

-- 아래는 생성자가 인자를 가지고 있는 형태, 열거형이 아님
data Colour = Black | Red | Green | RGB Int Int Int

-- Bool은 '우연히도' 열거형
data Bool = False | True
  deriving (Eq, Ord, Enum, Read, Show, Bounded)

---------------------------------------------------------------
--
-- parameterized data type
--
---------------------------------------------------------------

data Pair a b = Pair a b

xx = Pair 1 2
yy = Pair "Howdy" 42

first :: Pair a b -> a
first (Pair x _) = x

apply :: (a -> a') -> (b -> b') -> Pair a b -> Pair a' b'
apply f g (Pair x y) = Pair (f x) (g y)

---------------------------------------------------------------

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