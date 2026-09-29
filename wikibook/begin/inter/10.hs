-- functor ~~ mappable
{- instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x) -}

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)
instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)
    -- functors have two laws (really should follow!)
    -- fmap id x = id x
    -- fmap (g . f) -> fmap g . fmap f