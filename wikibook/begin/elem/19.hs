import Distribution.Simple.Utils
insensitive x y = compare (lowercase x) (lowercase y)

quickSort' :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
-- No matter how we compare two things the base case doesn't change,
-- so we use the _ "wildcard" to ignore the comparison function.
quickSort' _ [] = []

-- c is our comparison function
quickSort' c (x : xs) = quickSort' c less ++ (x : equal) ++ quickSort' c more
    where
        less  = filter (\y -> y `c` x == LT) xs
        equal = filter (\y -> y `c` x == EQ) xs
        more  = filter (\y -> y `c` x == GT) xs

main = print (quickSort' insensitive ["for", "have", "this", "a", "I", "Linux"])