applyToIntegers :: (Integer -> Integer) -> [Integer] -> [Integer]
applyToIntegers _ [] = []
applyToIntegers f (n:ns) = f n : applyToIntegers f ns
multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList m = applyToIntegers (m *)
doubleList = multiplyList 2
tripleList = multiplyList 3
heads :: [[a]] -> [a]
heads = map head