data Tree a = Node a [Tree a]
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Node v c) = Node (f v) (map (fmap f) c)
