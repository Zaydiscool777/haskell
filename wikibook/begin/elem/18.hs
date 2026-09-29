plusTwo = (+ 2) -- this is called sectioning
six = plusTwo 4
elem' :: (Eq a) => a -> [a] -> Bool
x `elem'` xs = any (==x) xs -- defining a function in infix
