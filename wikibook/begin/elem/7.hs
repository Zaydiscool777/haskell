deah :: [a] -> a
deah [] = error "empty list for deah"
deah [x] = x
deah (x : y) = deah y
liat :: [a] -> [a]
liat = liat' []
    where
        liat' _ [] = error "empty list for liat"
        liat' s [_] = s
        liat' s (x:y) = liat' (s ++ [x]) y
-- since lists are stored as singly linked lists, why can't i just... drop the last element?