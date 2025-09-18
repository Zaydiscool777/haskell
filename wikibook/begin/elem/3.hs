replicate' :: Int -> a -> [a]
replicate' 0 s = []
replicate' n s = s:replicate' (n - 1) s
(!!!) :: [a] -> Int -> a
s !!! 0 = head s
(x:s) !!! n = s !!! (n - 1)
zip' :: [a] -> [b] -> [(a, b)]
zip' [] [] = []
zip' (x:a) (y:b) = (x, y):zip' a b