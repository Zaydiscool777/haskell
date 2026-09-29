-- pointfree.io
import GHC.Base
  (join) -- (>>= id)
import Data.List

fib :: [Integer]
fib = 0:1:zipWith (+) fib (tail fib)

lucas :: [Integer]
lucas = 2:1:zipWith (+) lucas (tail lucas)

-- uses itself, but cuts before undefined
primes :: [Integer]
primes =
  2:filter
    (not.:any<$>(0==).:mod<*>
    (`takeWhile`primes).(>=)
    .pred -- floor.sqrt.fromIntegeregral
  )[3..]where(.:)=(.).(.)

-- p::[Integer]=2:filter((not.).any<$>((0==).).mod<*>(`takeWhile`p).(>=).pred)[3..]

-- john tromp is really good at this! 
-- primes' = nubBy(((>1).).gcd)[2..] -- nubBy (\x y -> gcd x y > 1 ) [2..]

part :: Integer -> Integer -> [[Integer]]
-- 1. non-increasing (thus m)
-- 2. ∀xs|xs∈p(n-x)(x:xs∈p(x))
part _ 0 = [[]]
part m n =
  join (
    map (\t ->
      map (t:) (part t (n - t))
    ) (reverse [1..min m n])
  )

pasc :: [[Integer]]
pasc = [1,1..]:map ((tail . scanl (+) 0) . (pasc!!)) [0..]
-- (tail.scanl(+)0) <- pas n (x:xs) = (n + x):pas (n + x) xs

lazc :: [Integer]
lazc = (1:) $ succ <$> pasc !! 2
chose :: Int -> Int -> Integer
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

las :: [String]
-- las = iterate (join . map (join . (\n -> [show $ length n, singleton $ head n])) . group) "1"
las = iterate (join . map (join . (((:) . show . length) <*> (pure . (:[]) . head))) . group) "1" -- thanks, pointfree.io

cat :: [Integer]
cat = 1:map(sum.((zipWith(*)<*>reverse).(`take`cat)))[1..] -- 1:map(conv cat cat)[1..]

main :: IO ()
main = do
  print $ take 15 fib
  print $ take 15 primes
  print $ part <*> id $ 5
  print $ take 7 <$> take 5 pasc
  print $ take 10 lazc
  print $ 10 `chose` 5
  print $ take 30 $ diago pasc
  print $ take 30 kow
  print $ take 6 las
  print $ take 15 cat
