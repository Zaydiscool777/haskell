-- USE FOLDR TO IMPLEMENT FOLDL
foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' f acc xs = foldr (flip f) acc (reverse xs)

foldl_ :: (b -> a -> b) -> b -> [a] -> b
foldl_ = foldr . flip -- i don't trust this