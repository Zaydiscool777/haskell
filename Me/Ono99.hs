-- https://wiki.haskell.org/index.php?title=99_questions/
import System.CPUTime (getCPUTime) -- timer
import Control.Exception (evaluate) -- timer
import System.Random (randomRIO) -- -package random
import qualified Data.Map as M -- -package containers
import Data.Bifunctor (first, second)
import Data.List (findIndex, sort, sortBy, sortOn, uncons)
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import Data.Tree as T -- -package containers
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
nlflatten :: NestedList a -> [a]
nlflatten (Elem x) = [x]
nlflatten (List a) = concatMap nlflatten a

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

data MyBTRee a = MyLeaf {val :: Int, get :: a} | MyBranch {val :: Int, left :: MyBTRee a, right :: MyBTRee a}
huffman :: [(a, Int)] -> [(a, String)]
huffman = listHuff . makeHuff . sortOn val . map (uncurry $ flip MyLeaf)
  where
    makeHuff :: [MyBTRee a] -> MyBTRee a
    makeHuff [] = error "empty list of MyBTRees"; makeHuff [x] = x
    makeHuff (a:b:r) = makeHuff $ sortOn val (MyBranch (val a + val b) a b : r)
    listHuff :: MyBTRee a -> [(a, String)]
    listHuff (MyLeaf _ g) = [(g, "")]
    listHuff (MyBranch _ l r) = map (second ('0':)) (listHuff l) ++ map (second ('1':)) (listHuff r)
-- 51 to 53 do not exist
-- 54 to 60
data TRee a = Empty | Branch a (TRee a) (TRee a) deriving (Show, Eq)
leaf :: a -> TRee a
leaf x = Branch x Empty Empty
-- 54A would check for valid trees, but its type system forces trees to be valid

cBalTRee :: Int -> [TRee Char]
cBalTRee 0 = [Empty]; cBalTRee 1 = [leaf 'x']
cBalTRee x
  | odd x = branches (cart (cBalTRee h) (cBalTRee h))
  | otherwise = branches (cart (cBalTRee h) (cBalTRee i)) ++ branches (cart (cBalTRee i) (cBalTRee h))
  where h = pred x `div` 2; i = succ h; cart xs ys = [(x, y) | x <- xs, y <- ys]; branches = map (uncurry (Branch 'x'))

symmetric :: (Eq a) => TRee a -> Bool
symmetric x = isMirror x x
  where
    isMirror :: (Eq a) => TRee a -> TRee a -> Bool
    isMirror Empty Empty = True
    isMirror Empty (Branch {}) = False
    isMirror (Branch {}) Empty = False
    isMirror (Branch _ al ar) (Branch _ bl br) = isMirror al br && isMirror ar bl

constTRee :: (Ord a) => [a] -> TRee a
constTRee = foldr addTRee Empty . reverse
  where
    addTRee :: (Ord a) => a -> TRee a -> TRee a
    addTRee x Empty = leaf x
    addTRee x (Branch v l r)
      | x <= v = Branch v (addTRee x l) r
      | otherwise = Branch v l (addTRee x r)

cSymBal :: Int -> [TRee Char]
cSymBal = filter symmetric . cBalTRee
-- failed, but i *think* i understand it now
hBalTRee :: a -> Int -> [TRee a]
hBalTRee x 0 = [Empty]
hBalTRee x 1 = [leaf x]
hBalTRee x h = [Branch x l r |
  (hl, hr) <- [(j, i), (i, i), (i, j)],
  l <- hBalTRee x hl, r <- hBalTRee x hr]
    where i = pred h; j = pred i
-- failed, but i knew minNodes was related to fib
hbalTReeNodes :: a -> Int -> [TRee a]
hbalTReeNodes _ 0 = [Empty]
hbalTReeNodes x n = concatMap toFilteredTRees [minHeight..maxHeight]
  where
    toFilteredTRees = filter ((n==) . countNodes) . hBalTRee x
    minNodesSeq = 0:1:zipWith ((+).(1+)) minNodesSeq (tail minNodesSeq)
    minNodes = (minNodesSeq !!)
    minHeight = ceiling $ logBase 2 $ fromIntegral (succ n)
    maxHeight = pred (fromJust $ findIndex (n<) minNodesSeq)
    countNodes Empty = 0
    countNodes (Branch _ l r) = succ (countNodes l + countNodes r)
-- 61-69
countLeaves :: TRee a -> Int
countLeaves Empty = 0; countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ a b) = countLeaves a + countLeaves b
trleaves :: TRee a -> [a] -- 61A
trleaves Empty = []; trleaves (Branch x Empty Empty) = [x]
trleaves (Branch _ a b) = trleaves a ++ trleaves b

internals :: TRee a -> [a]
internals Empty = []; internals (Branch _ Empty Empty) = [];
internals (Branch v a b) = v : internals a ++ internals b
atLevel :: TRee a -> Int -> [a] -- 62B
atLevel Empty _ = []; atLevel (Branch v _ _) 0 = [v]
atLevel (Branch _ a b) x = atLevel a (pred x) ++ atLevel b (pred x)

compTRee :: Int -> TRee Char
compTRee x = Empty -- !
isComp :: TRee a -> Bool
isComp Empty = True
isComp (Branch _ l r) = abs (len l - len r) <= 1 && isComp l && isComp r
  where
    len Empty = 0
    len (Branch _ a b) = succ (max (len a) (len b))

layout1 :: TRee a -> [(a, Int, Int)]
layout1 x = zipWith (curry (\(i, (a, d)) -> (a, i, succ d))) [1..] (lnDepth x)
  where
    lnDepth :: TRee a -> [(a, Int)]
    lnDepth Empty = []
    lnDepth (Branch v l r) = upChild l ++ (v, 0) : upChild r
    upChild = map (second succ) . lnDepth

layout2 :: TRee a -> [(a, Int, Int)]
layout2 x = map (\(a, x, y) -> (a, x + minsnd, negate y)) (draw x sp (height x))
  where
    minsnd = negate (minimum (map (\(_, a, _) -> a) (draw x sp (height x))))
    height :: TRee a -> Int -- get maximum height
    height Empty = 0
    height (Branch _ l r) = succ (max (height l) (height r))
    sp = 2 ^ pred (pred (height x))
    draw :: TRee a -> Int -> Int -> [(a, Int, Int)]
    draw Empty _ _ = []
    draw (Branch v l r) s h = [(v, 0, 0)]
      ++ map (\(a, x, y) -> (a, x - s, pred y)) (draw l (s `div` 2) (h - 1))
      ++ map (\(a, x, y) -> (a, x + s, pred y)) (draw r (s `div` 2) (h - 1))
-- failed 66. the solution also breaks.
parseSTRee :: String -> TRee String -- note: exercise wants Maybe (TRee String), where Nothing is for invalid input
parseSTRee "" = Empty
parseSTRee z = Branch t (parseSTRee l) (parseSTRee r)
  where
    stail = maybe "" snd . uncons; sinit "" = ""; sinit x = init x
    (t, a) = span ('('/=) z -- "abc(bla,)" -> ("abc", "(bla,)")
    b = (stail . sinit) a -- "(bla,)" -> "bla,"
    (l, r) = go "" b 0 -- "bla," -> ("bla", "")
    go :: String -> String -> Int -> (String, String)
    go x "" _ = ("", "") -- not (x, "")
    go x (',':r) 0 = (reverse x, r)
    go x ('(':r) n = go ('(':x) r (succ n)
    go x (')':r) n = go (')':x) r (pred n)
    go x (y:r) n = go (y:x) r n
parseTReeS :: TRee String -> String
parseTReeS Empty = ""
parseTReeS (Branch v Empty Empty) = v
parseTReeS (Branch v l r) = v ++ '(' : parseTReeS l ++ ',' : parseTReeS r ++ ")"

preorder :: TRee a -> [a]
preorder Empty = []; preorder (Branch v l r) = v : preorder l ++ preorder r
inorder :: TRee a -> [a]
inorder Empty = []; inorder (Branch v l r) = inorder l ++ v : inorder r
postorder :: TRee a -> [a] -- bonus!
postorder Empty = []; postorder (Branch v l r) = postorder l ++ postorder r ++ [v]
-- instead of omitting null, make it a seperate character. (a.k.a. exercise 69)
constPreIn :: (Eq a) => [a] -> [a] -> TRee a
{-
given the preorder and inorder traversals of a binary tree, if all elements are unique, we can construct the tree.
preorder: root, left, right
inorder: left, root, right
we can take the first element of preorder as root, then split inorder at that element to get left and right subtrees.
since the element after it the first preorder part of the right subtree,
we can use it to split preorder into left and right subtrees, and run recursively.
-} -- this also works with postorder, just take the last instead of first
constPreIn [] [] = Empty; constPreIn x y | length x /= length y = error "different sizes"
constPreIn p i = Branch v l r
  where
    v = head p
    (a, _:b) = span (/=v) (tail p)
    (d, e) = span (/=v) i
    (l, r) = (constPreIn a d, constPreIn b e)
constPrePost :: (Eq a) => [a] -> [a] -> TRee a -- bonus!
{-
we can first remove the first of preorder and the last of postorder, which is the root.
now, the last of postorder is the first of the right branch in preorder,
and the last of preorder is the first of the right branch in postorder.
because of this, we can split the left and right subtrees, and run recursively.
-}
constPrePost [] [] = Empty; constPrePost x y | length x /= length y = error "different sizes"
constPrePost p o = Branch v l r
  where
    v = head p
    (a, b) = (tail p, init o)
    (sp, so) = (last b, last a)
    (c, d) = span (sp/=) a
    (e, f) = span (so/=) b
    l = constPrePost c e
    r = constPrePost d f

parseDTRee :: String -> TRee Char
parseDTRee "." = Empty; parseDTRee "" = Empty
parseDTRee (v:x) = Branch v (parseDTRee l) (parseDTRee r)
  where
    a:b = x
    (l, c:d) = go [a] b 2
    (r, _) = go [c] d 2
    go :: String -> String -> Int -> (String, String)
    go x "" _ = ("", "") -- not (x, "")
    go x r 0 = (reverse x, r)
    go x ('.':r) n = go ('.':x) r (pred n)
    go x (y:r) n = go (y:x) r (succ n)
parseTReeD :: TRee Char -> String
parseTReeD Empty = "."
parseTReeD (Branch v l r) = v : parseTReeD l ++ parseTReeD r
-- 70-73
-- 70B is just like 54A but for multiway trees (Data.Tree as T)

nnodes :: Tree a -> Int -- 70C
nnodes (Node _ x) = succ (sum (map nnodes x))
-- failed. it seemed easy, but i guess this was a more imperative exercise
parseSTree :: String -> Tree Char
parseSTree (x:"^") = Node  x  []
parseSTree (x:xs) = Node  x  ys
  where
    z = map fst $ filter ((==) 0 . snd) $ zip [0..] $
      scanl (+) 0 $ map (\x -> if x == '^' then -1 else 1) xs
    ys = zipWith (curry (parseSTree . uncurry (sub xs))) (init z) (tail z)
    sub s a b = take (b - a) $ drop a s
parseTreeS :: Tree Char -> String -- i did this, but it was pretty easy
parseTreeS (Node v c) = v : concatMap parseTreeS c ++ "^"

ipl :: Tree a -> Int
ipl = len
  where
    len (Node _ []) = 0
    len (Node _ c) = sum (map (succ . len) c)


---------------------------------------------------

test = "abcdefghi"
test2 = "aaabccddaddee"
test3 = ["ax", "bx", "cx", "defg", "h", "j", "lmn"]
test4 = 4268880 -- 66389621760
test5 = constTRee [5, 3, 18, 1, 4, 12, 21]

timer :: a -> IO (Double, a)
timer action = do
    start <- getCPUTime
    result <- evaluate action
    end <- getCPUTime
    let diff = fromIntegral (end - start) / 10^12 :: Double
    return (diff, result)

print' :: (Show a) => a -> IO ()
print' = putStr . show

printM :: (Show a) => IO a -> IO ()
printM = (>>= print)

main :: IO ()
main = do
  print $ myLast test
  print $ myButLast test
  print $ elementAt test 6
  print $ myLength test
  print $ myReverse test
  print $ isPalindrome "amanaplanacanalpanama"
  print . nlflatten $ List [Elem 1, List [Elem 3, Elem 2], Elem 4]
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
  printM $ rndSelect test 5
  printM $ lottoSelect 6 50
  printM $ rndPermu test
  print $ combinations 2 [1..4]
  print' $ group3 test !! 792; print $ group' [2,2,5] test !! 729
  print' $ lsort test3; print $ lfsort test3
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
  print $ cBalTRee 4
  print . symmetric $ Branch 'x' (leaf 'y') (leaf 'y')
  print . symmetric $ test5
  print $ cSymBal 5
  print . length $ hBalTRee undefined 4
  print . length $ hbalTReeNodes undefined 10 -- haskell is very lazy
  print' . countLeaves $ test5; print . trleaves $ test5
  print' . internals $ test5; print $ atLevel test5 1
  -- print' $ compTRee 5; print $ isComp (compTRee 50) -- fix compTRee!
  print $ layout1 test5
  print $ layout2 test5
  -- 66 can't be put here
  print $ (\x -> parseTReeS (parseSTRee x) == x) "a(b(d,e),c(,f(g,)))" -- note: exercise wants Maybe (TRee String), where Nothing is for invalid input
  --print $ 
  --print $ 
