power' x y
    | y == 0 = 1
    | otherwise = x * power' x (y - 1)
addition x y
    | x == 0 = y
    | otherwise = addition (x - 1) (plusOne y)
    where plusOne x = x + 1
log2' :: Int -> Int
log2' x = log2b' x 0
    where
        log2b' :: Int -> Int -> Int
        log2b' x y
            | x == 1 = y
            | otherwise = log2b' (x `div` 2) (y + 1)
