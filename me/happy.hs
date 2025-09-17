-- rosettacode

happy x = sum (map (\x -> x * x) (digits x))
  where
    digits 0 = [0]
    digits x = (x `mod` 10):(digits (x `div` 10))

ishappy x = floyd (happy x) (happy (happy x))
  where
    floyd :: Int -> Int -> Bool
    floyd 1 _ = True
    floyd _ 1 = True
    floyd a b | a == b = False
    floyd x y = floyd (happy x) (happy (happy y))

main = print $ sum $ take 10000 $ filter ishappy [1..]
