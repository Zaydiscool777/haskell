{- mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x:xs) = x + mySum xs -}
{- mySum = foldr (+) 0
myProduct = foldl (*) 1 -}
-- because haskell is lazy, this will eat memory
-- foldl' from Data.List is strict, so it won't
-- foldr1 and foldl1 take the last element as acc (0/1 up there)
-- example of why foldr is more haskell-like than foldl:
echoes :: [Int] -> [Int]
echoes = foldr (\ x xs -> replicate x x ++ xs) []
laziness = take 10 (echoes [1..])
-- use foldr if infinite, use foldl(') if finite
-- or: use foldr if creating a structure, use foldl(') if reducing to a value