-- pointfree.io
import GHC.Base
  (join) -- (>>= id)
import Data.List

fib :: [Int]
fib = 0:1:zipWith (+) fib (tail fib)

lucas :: [Int]
lucas = 2:1:zipWith (+) lucas (tail lucas)

primes :: [Int]
primes =
  2:filter
    ((not.).any<$>(((0==).).mod)<*>
    ((`takeWhile`primes).(>=)
    .floor.sqrt.fromIntegral)
  ) [3..]

part :: Int -> Int -> [[Int]]
-- 1. non-increasing
-- 2. ∀xs|xs∈p(n-x)[x:xs∈p(x)]
part _ 0 = [[]]
part m n =
  join (
    map (\t ->
      map (t:) (part t (n - t))
    ) (reverse [1..min m n])
  )

pasc :: [[Int]]
pasc = [1,1..]:map ((tail.scanl (+) 0).(pasc!!)) [0..]
-- (tail..0) <- pas n (x:xs) = (n + x):pas (n + x) xs

lazc :: [Int]
lazc = (1:) $ succ <$> pasc !! 2
chose :: Int -> Int -> Int
chose = join . (((!!) . (pasc !!)) .) . (-)

diago :: [[a]] -> [a]
diago x = diag 0 0 True x
  where
    diag a b c x = (x!!a!!b):next a b c x
    preed x = if x == 0 then 0 else pred x
    next a b True =
      diag (succ a) (preed b) (b>0)
    next a b False =
      diag (preed a) (succ b) (a==0)

-- and his name is John Tromp!!!
-- but not me
kow :: [Int]
kow = 1:2:drop 2 (concat . zipWith replicate kow . cycle $ [1, 2])

say :: String -> String
say = join . map (join . (\n -> [show $ length n, singleton $ head n])) . group

main :: IO ()
main = do
  print $ take 10 fib
  print $ take 10 primes
  print $ part 4 4
  print $ take 5 <$> take 10 pasc
  print $ take 10 lazc
  print $ 10 `chose` 5
  print $ take 30 $ diago pasc
  print $ take 30 kow
  --look and say
  --catalan numbers



