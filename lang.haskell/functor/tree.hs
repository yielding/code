data Tree a
   = Empty
   | Leaf a
   | Node (Tree a) a (Tree a)
   deriving (Show)

instance Functor Tree where
   fmap :: (a -> b) -> Tree a -> Tree b
   fmap _ Empty        = Empty
   fmap f (Leaf v)     = Leaf $ f v
   fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

main :: IO()
main = do
   let tree = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7)) 
   print $ id <$> tree
   print $ (+1) . (*2) <$> tree
