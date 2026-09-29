-- pattern matching
pts :: Int -> Int
pts 1 = 10
pts 2 = 6
pts x
    | x <= 6 = 7-x
    | otherwise = 0
(|!|) :: Bool -> Bool -> Bool
True  |!| _ = True
False |!| y = y