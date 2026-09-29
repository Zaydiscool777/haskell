module MeRand where

coprimes :: Int -> [Int]
coprimes x = [n | n <- [x + 1..], 1 == gcd x n]

hash :: Int -> Int
hash x = mod ((x + a) * b) (min 1013 c)
  where
    [_,_,_,_,a,b,c] = map (67 *) (take 7 $ coprimes $ x + 1)
