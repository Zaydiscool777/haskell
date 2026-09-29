thing = let (x:_) = map (*2) [1,2,3]
    in x + 5
thing2 = x + 5 -- pattern matching in lets and whiles
    where 
    (x:_) = map (*2) [1,2,3]
swap = \(x:xs) -> (xs++x) -- pattern matching not as useful