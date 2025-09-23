-- https://wiki.haskell.org/index.php?title=99_questions/
import System.CPUTime (getCPUTime) -- timer
import Control.Exception (evaluate) -- timer
import System.Random (randomRIO) -- -package random
import qualified Data.Map as M -- -package containers
import Data.Bifunctor (first, second)
import Data.List (sort, sortOn, sortBy, findIndex)
import Data.Tuple (swap)
import Data.Maybe (fromJust)
-- 1-10

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast x = myLast $ tail x

myButLast :: [a] -> a
myButLast [x] = x; myButLast [] = error "empty list"
myButLast [x,_] = x
myButLast x = myButLast $ tail x

elementAt :: (Eq t, Num t, Enum t) => [a] -> t -> a
elementAt [] _ = error "empty list"
elementAt x 0 = head x
elementAt x y = elementAt (tail x) (pred y)

myLength :: [a] -> Int
myLength [] = 0
myLength x = succ $ myLength (tail x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:y) = myReverse y ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = myReverse x == x

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List a) = concatMap flatten a

compress :: Eq a => [a] -> [a]
compress (a:b:r)
  | a == b = compress (a:r)
  | otherwise = a:compress (b:r)
compress x = x
-- failed
pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (first,rest) = span (==x) xs
  in (x:first):pack rest
pack [] = []

encode :: (Eq a) => [a] -> [(Int, a)]
encode x = zip (map myLength (pack x)) (compress x)
-- 11-20
data Plurality a = Multiple Int a | Single a deriving (Show, Eq)
encodeModified :: (Eq a) => [a] -> [Plurality a]
encodeModified l = map (\x ->
  if fst x == 1 then Single (snd x) else uncurry Multiple x)
  (encode l)

decodeModified :: [Plurality a] -> [a]
decodeModified [] = []
decodeModified ((Single a):r) = a:decodeModified r
decodeModified ((Multiple n a):r) = replicate n a ++ decodeModified r

encodeDirect :: Eq a => [a] -> [Plurality a]
encodeDirect [] = []
encodeDirect (a:r) = let (c,s) = span (a==) r in
  if null c then
    Single a:encodeDirect r
  else
    Multiple (succ $ length c) a:encodeDirect s

dupli :: [b] -> [b]
dupli = flip repli 2 -- just repli but y=2

repli :: [b] -> Int -> [b]
repli x y = concatMap (replicate y) x

dropEvery :: [a] -> Int -> [a]
dropEvery _ x | x <= 0 = error "not natural number"
dropEvery [] _ = []
dropEvery x n = take (n - 1) x ++ dropEvery (drop n x) n

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split x 0 = ([], x)
split (a:r) n = let (b,s) = split r (pred n) in (a:b, s)

slice :: [a] -> Int -> Int -> [a]
slice l a b = take a (drop b l)

rotate :: [a] -> Int -> [a]
rotate l n
  | n < 0 = drop (-n) l ++ take (-n) l
  | otherwise = drop (length l - n) l ++ take (length l - n) l
-- 21-28
removeAt :: Int -> [a] -> (a, [a])
removeAt x y | x > length y = (head y, [])
removeAt x y = (elementAt y x, take x y ++ drop (succ x) y)

insertAt :: a -> [a] -> Int -> [a]
insertAt x l n = take n l ++ x : drop n l

range :: (Eq t, Enum t) => t -> t -> [t]
range x y | x == y = [x]
range x y = x:range (succ x) y

rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = pure []
rndSelect _ x | x <= 0 = pure []
rndSelect l n = do
  i <- randomRIO (0, pred $ length l) -- randomRIO is an artifact
  let m = removeAt i l
  j <- rndSelect (snd m) (pred n)
  return (fst m:j)

lottoSelect :: Int -> Int -> IO [Int]
lottoSelect = flip $ rndSelect . range 1

rndPermu :: [a] -> IO [a]
rndPermu x = rndSelect x (length x)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]; combinations x y | x > length y = []
combinations n (i:r) = -- 2 "abc"
  map (i:) (combinations (pred n) r) -- 'a':1 "bc" -> "ab" "ac"
  ++ combinations n r -- 2 "bc" -> "bc"

group3 :: [a] -> [[[a]]] -- 9 -> [2, 3, 4] combs
group3 xs =
  [[as, bs, cs] | -- this part was ai
    (as, rest) <- combLeft 2 xs,
    (bs, cs) <- combLeft 3 rest]
combLeft :: Int -> [a] -> [([a], [a])] -- do i get half credit for this?
combLeft 0 x = [([], x)]
combLeft x y | x > length y = []
combLeft n (i:r) = map (first (i:)) (combLeft (pred n) r) ++ map (second (i:)) (combLeft n r)
group' :: [Int] -> [a] -> [[[a]]]
group' [] _ = [[]]
group' (n:r) x =
  [as:next |
    (as, rest) <- combLeft n x,
    next <- group' r rest]

lsort :: [[a]] -> [[a]]
lsort = sortOn length
lfsort :: [[a]] -> [[a]] -- failed
lfsort l = sortBy (\xs ys -> compare (frequency (length xs) l) (frequency (length ys) l)) l
  where frequency len l = length (filter (\x -> length x == len) l)
-- 29 and 30 do not exist
-- 31-41
isPrime :: Int -> Bool
isPrime x = all (\n -> x `mod` n /= 0) [2..(pred x)]

myGCF :: Int -> Int -> Int
myGCF a b = if r == 0 then q else myGCF b r
  where
    q = div a b
    r = mod a b

coprime :: Int -> Int -> Bool
coprime = ((1==) .) . gcd -- or (1==) .: gcd where .: = (.) . (.)

totient :: Int -> Int
totient x = length (filter id (map (coprime x) [1..(pred x)]))

primeFactors :: Int -> [Int]
primeFactors y = if null x then [] else head x:primeFactors (div y (head x))
  where x = filter ((0 ==) . mod y) [2..(pred y)]

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map swap . encode . primeFactors

totientMult :: [(Int, Int)] -> Int
totientMult y = product (map (\x -> pred (fst x) * fst x ^ pred (snd x)) y)

timingTotients :: IO ()
timingTotients = do
  (t1, v1) <- timer (totient test4)
  -- putStrLn $ "totient: " ++ show v1 ++ ", time: " ++ show t1
  (t2, v2) <- timer (totientMult $ primeFactorsMult test4)
  -- putStrLn $ "totientMult . encode: " ++ show v2 ++ ", time: " ++ show t2
  putStrLn $ "totientMult in times faster than totient: " ++ show (t1 / t2)

primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a..b]

goldbach :: Int -> (Int, Int)
goldbach x | odd x || x < 2 = (0, 0)
goldbach y = isGoldbach [2..y]
  where
    isGoldbach [] = error "bro disproved goldbach's theorem with a 32-bit integer"
    isGoldbach (x:r)
      | isPrime x && isPrime (y - x) = (x, y - x)
      | otherwise = isGoldbach r

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList x y = filter ((0/=) . fst) $ map goldbach [x..y]
goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' x y z = filter ((z<=) . fst) $ goldbachList x y
-- 42 to 45 do not exist
-- 46-50
and', or', nand', nor', xor', imp', equ' :: Bool -> Bool -> Bool
(and', or', nand', nor', xor', imp', equ') =
  (\x y -> if x then y else x,
  \x y -> if x then x else y,
  (not .) . and', (not .) . or',
  \x y -> and' (nand' x y) (or' x y),
  flip (.) not . nand', -- point-free flex
  \x y -> if x then y else not y)
table :: (Bool -> Bool -> Bool) -> String
table x = tablen 2 (\[a,b] -> x a b)

-- 47 is to make them operators, but we can use `infix` notation. use infixl if you want

tablen :: Int -> ([Bool] -> Bool) -> String
tablen n x = unlines $ map ((unwords . map show) . (\l -> l ++ [x l])) $ permBool n
  where permBool 1 = [[True], [False]]; permBool n = map (True:) (permBool $ pred n) ++ map (False:) (permBool $ pred n)

gray :: Int -> [String]
gray 1 = ["0", "1"]; gray n = map ('0':) x ++ map ('1':) (reverse x) where x = gray (pred n)

data MyBTree a = MyLeaf {val :: Int, get :: a} | MyBranch {val :: Int, left :: MyBTree a, right :: MyBTree a}
huffman :: [(a, Int)] -> [(a, String)]
huffman = listHuff . makeHuff . sortOn val . map (uncurry $ flip MyLeaf)
  where
    makeHuff :: [MyBTree a] -> MyBTree a
    makeHuff [] = error "empty list of MyBTrees"; makeHuff [x] = x
    makeHuff (a:b:r) = makeHuff $ sortOn val (MyBranch (val a + val b) a b : r)
    listHuff :: MyBTree a -> [(a, String)]
    listHuff (MyLeaf _ g) = [(g, "")]
    listHuff (MyBranch _ l r) = map (second ('0':)) (listHuff l) ++ map (second ('1':)) (listHuff r)
-- 51 to 53 do not exist
-- 54 to 60
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Branch x Empty Empty
-- 54A would check for valid trees, but its type system forces trees to be valid

cBalTree :: Int -> [Tree Char]
cBalTree 0 = [Empty]; cBalTree 1 = [leaf 'x']
cBalTree x
  | odd x = branches (cart (cBalTree h) (cBalTree h))
  | otherwise = branches (cart (cBalTree h) (cBalTree i)) ++ branches (cart (cBalTree i) (cBalTree h))
  where h = pred x `div` 2; i = succ h; cart xs ys = [(x, y) | x <- xs, y <- ys]; branches = map (uncurry (Branch 'x'))

symmetric :: (Eq a) => Tree a -> Bool
symmetric x = isMirror x x
  where
    isMirror :: (Eq a) => Tree a -> Tree a -> Bool
    isMirror Empty Empty = True
    isMirror Empty (Branch {}) = False
    isMirror (Branch {}) Empty = False
    isMirror (Branch _ al ar) (Branch _ bl br) = isMirror al br && isMirror ar bl

constTree :: (Ord a) => [a] -> Tree a
constTree = foldr addTree Empty . reverse
  where
    addTree :: (Ord a) => a -> Tree a -> Tree a
    addTree x Empty = leaf x
    addTree x (Branch v l r)
      | x <= v = Branch v (addTree x l) r
      | otherwise = Branch v l (addTree x r)

cSymBal :: Int -> [Tree Char]
cSymBal = filter symmetric . cBalTree
-- failed, but i *think* i understand it now
hBalTree :: a -> Int -> [Tree a]
hBalTree x 0 = [Empty]
hBalTree x 1 = [leaf x]
hBalTree x h = [Branch x l r |
  (hl, hr) <- [(j, i), (i, i), (i, j)],
  l <- hBalTree x hl, r <- hBalTree x hr]
    where i = pred h; j = pred i
-- failed, but i knew minNodes was related to fib
hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes x n = concatMap toFilteredTrees [minHeight..maxHeight]
  where
    toFilteredTrees = filter ((n==) . countNodes) . hBalTree x
    minNodesSeq = 0:1:zipWith ((+).(1+)) minNodesSeq (tail minNodesSeq)
    minNodes = (minNodesSeq !!)
    minHeight = ceiling $ logBase 2 $ fromIntegral (succ n)
    maxHeight = pred (fromJust $ findIndex (n<) minNodesSeq)
    countNodes Empty = 0
    countNodes (Branch _ l r) = succ (countNodes l + countNodes r)
-- 61-69
countLeaves :: Tree a -> Int
countLeaves Empty = 0; countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ a b) = countLeaves a + countLeaves b
leaves :: Tree a -> [a] -- 61A
leaves Empty = []; leaves (Branch x Empty Empty) = [x]
leaves (Branch _ a b) = leaves a ++ leaves b

-- internal nodes are basically everything not leaves

---------------------------------------------------

test = "abcdefghi"
test2 = "aaabccddaddee"
test3 = ["ax", "bx", "cx", "defg", "h", "j", "lmn"]
test4 = 4268880 -- 66389621760
test5 = constTree [5, 3, 18, 1, 4, 12, 21]

timer :: a -> IO (Double, a)
timer action = do
    start <- getCPUTime
    result <- evaluate action
    end <- getCPUTime
    let diff = fromIntegral (end - start) / 10^12 :: Double
    return (diff, result)

main :: IO ()
main = do
  print $ myLast test
  print $ myButLast test
  print $ elementAt test 6
  print $ myLength test
  print $ myReverse test
  print $ isPalindrome "amanaplanacanalpanama"
  print . flatten $ List [Elem 1, List [Elem 3, Elem 2], Elem 4]
  print $ compress test2
  print $ pack test2
  print $ encode test2
  print $ encodeModified test2
  print . decodeModified $ encodeModified test2
  print $ encodeDirect test2
  print $ dupli test
  print $ repli test 5
  print $ dropEvery test 3
  print $ split test 5
  print $ slice test 5 8
  print $ removeAt 3 test2
  print $ insertAt 'z' test2 5
  print $ range (-3) 7
  rndSelect test 5 >>= print
  lottoSelect 6 50 >>= print
  rndPermu test >>= print
  print $ combinations 2 [1..4]
  putStr . show $ group3 test !! 792; print $ group' [2,2,5] test !! 729
  putStr . show $ lsort test3; print $ lfsort test3
  -- 29-30 does not exist
  -- 29-30 does not exist
  print $ isPrime 4327
  print $ myGCF 1071 462
  print $ coprime 35 64
  print $ totient 10
  print $ primeFactors 5040
  print $ primeFactorsMult 5040
  print . totientMult $ primeFactorsMult 1000
  --timingTotients
  print $ goldbach 3234
  --print $ goldbachList' 2 3000 50
  -- 42 does not exist
  -- 43 does not exist
  -- 44 does not exist
  -- 45 does not exist
  putStr $ table imp'
  putStr $ tablen 3 (foldr xor' False)
  -- 47 is infix definitions
  print $ gray 4
  print $ (huffman . map swap . encode) (sort test2)
  -- 51 does not exist
  -- 52 does not exist
  -- 53 does not exist
  -- 54 is redundant
  print $ cBalTree 4
  print $ symmetric (Branch 'x' (leaf 'y') (leaf 'y'))
  print . symmetric $ test5
  print $ cSymBal 5
  print . length $ hBalTree undefined 4
  print . length $ hbalTreeNodes undefined 10 -- haskell is very lazy
  putStr . show . countLeaves $ test5; print . leaves $ test5
  --print $ 
  --print $ 
  --print $ 
  --print $ 
