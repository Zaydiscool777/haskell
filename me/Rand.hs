module MeRand where

coprime :: Int -> Int -> Bool
coprime = (1 ==) .: gcd -- alternative: curry . (1 ==) . uncurry gcd -- any amount of args
  where (.:) = (.) . (.)

coprimes :: Int -> [Int] 
coprimes x = [n | n <- [x + 1..], coprime x n]

hash :: Int -> Int
hash x = mod ((x + a) * b) (min 1013 c)
  where
    (_:_:_:_:a:b:c:[]) = map (67 *) (take 7 $ coprimes $ x + 1)
