--
-- recursive data structure
--
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch left right) = Branch (treeMap f left) (treeMap f right)

-- wikidocs에서 말하는 것처럼 잘 안읽힘
treeMap2 :: (a -> b) -> Tree a -> Tree b
treeMap2 f = g where
  g (Leaf x) = Leaf (f x)
  g (Branch left right) = Branch (g left) (g right)


data List a = Nil | Cons a (List a)

main = do putStrLn "Hello"