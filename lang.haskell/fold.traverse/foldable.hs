import Data.Foldable

data Tree a 
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r


tree = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7))

main :: IO ()
main = do
  print $ fold [[1, 2], [3, 4]] -- [1, 2, 3, 4]
  print $ foldr (:) [] tree     -- [1, 2, 3, 4, 5, 6, 7]
  print $ length tree           -- 7

