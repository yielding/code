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

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Empty        = Empty
  fmap f (Leaf x)     = Leaf $ f x
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r

tree = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7))

main = traverse print tree
