{-# OPTIONS_GHC -Wno-x-partial #-}
-- https://wiki.haskell.org/index.php?title=H-99:_Ninety-Nine_Haskell_Problems
import System.CPUTime (getCPUTime)
import Control.Exception (evaluate)
import System.Random (randomRIO)
import qualified Data.Map as M
import Data.Bifunctor (first, second)
import Data.List
import Data.Tuple (swap)
import Data.Maybe
import Data.Tree (Tree(Node), drawForest)
import Control.Monad (foldM, join)
import Data.Ord (comparing)
import Data.Char (isLowerCase, isDigit)
import Control.Applicative ((<|>))

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
  where frequency len l = length [x | x <- l, length x == len]
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
totient x = length [s | s <- map (coprime x) [1..(pred x)], s]

primeFactors :: Int -> [Int]
primeFactors y = if null x then [] else head x:primeFactors (div y (head x))
  where x = [z | z <- [2..(pred y)], (y `mod` z) == 0]

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
compTRee 0 = Empty
compTRee x = Branch 'x' (compTRee (pred x)) (compTRee (pred x))
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
since the left of the element should have the same amount of nodes on its left in both orderings,
we can split the preorder according to that to get its left and right subtrees.
-} -- this also works with postorder, just take the last instead of first
constPreIn [] [] = Empty; constPreIn x y | length x /= length y = error "different sizes"
constPreIn p i = Branch v l r
  where
    (v, n) = fromJust $ uncons p
    (d, _:e) = span (/=v) i
    (a, b) = split n (length e)
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
parseDTRee = fst . go
  where
    go ('.':x) = (Empty, x)
    go (a:b) = first (Branch a (fst c)) d
      where
        c = go b
        d = go (snd c)
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

bottomUp :: Tree Char -> String
bottomUp (Node x y) = concatMap bottomUp y ++ [x]

displayL :: Tree Char -> String
displayL (Node x []) = [x]
displayL (Node x y) = '(' : x : concatMap ((' ':) . displayL) y ++ ")"

-- Data.Graph is in adjacency-list form, but it only supports Int.
-- despite the fact that there is no function that doesnt require Eq a, there is no way to enforce it either
data Graph a = Graph [a] [(a, a)] deriving (Eq, Ord, Show)
newtype GRaph a = GRaph [(a, [a])] deriving (Eq, Ord, Show)
-- suprisingly useful function
gadj :: Eq a => [(a, a)] -> a -> [a]; gadj b x = [i | (i, j) <- b, j == x] ++ [j | (i, j) <- b, i == x]
-- k in both of these is true if bidirectional and false if directional
graphToAdj :: Eq a => Graph a -> Bool -> GRaph a
graphToAdj (Graph a b) k = GRaph [(x, (if k then [i | (i, j) <- b, j == x] else []) ++ [j | (i, j) <- b, i == x]) | x <- a]
adjToGraph :: Eq a => GRaph a -> Bool -> Graph a
adjToGraph (GRaph x) k = Graph (map fst x) (if k then go [] (map fst x) x else [(a, c) | (a, b) <- x, c <- b])
  where
    go _ [] _ = []
    go z (x:r) y = [(x, a) | (a, b) <- y, x `elem` b, a `notElem` z] ++ go (x:z) r y

graphPaths :: Eq a => a -> a -> GRaph a -> [[a]]
graphPaths x y (GRaph z) = go [] x y z
  where
    go _ x y _ | x == y = [[x]]
    go w x y z = concatMap (\a -> map (x:) (go (x:w) a y z)) -- w blocks revisiting nodes
      ((filter (not . flip elem w) . snd . fromJust) (find ((x ==) . fst) z))

graphCycle :: Eq a => a -> GRaph a -> [[a]]
graphCycle x (GRaph z) = go False [] x x z
  where
    go :: Eq a => Bool -> [a] -> a -> a -> [(a, [a])] -> [[a]]
    go True _ x y _ | x == y = [[x]]
    go k w x y z = concatMap (\a -> map (x:) (go True (if k then x:w else []) a y z))
      ((filter (not . flip elem w) . snd . fromJust) (find ((x ==) . fst) z))

graphTrees :: Eq a => Graph a -> [Tree a] -- ai
graphTrees (Graph vertices edges) =
  concatMap (try edges) (filter (not . null) (subsets vertices))
  where
    subsets :: [a] -> [[a]]
    subsets []     = [[]]
    subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

    try :: Eq a => [(a, a)] -> [a] -> [Tree a]
    try _ [] = []
    try es vs =
      [ t
      | x <- subsets
          [ e | e@(a, b) <- es, a `elem` vs, b `elem` vs ]
      , length x == pred (length vs)
      , Just (t, seen) <- [grow [head vs] (head vs) x]
      , all (`elem` seen) vs
      ]

    grow :: Eq a => [a] -> a -> [(a, a)] -> Maybe (Tree a, [a])
    grow seen n es = do
      (seen', cs) <- foldM add (seen, []) (gadj es n)
      Just (Node n cs, seen')
      where
        add (visited, cs) next
          | next `elem` visited = Just (visited, cs)
          | otherwise = do
              (c, visited') <- grow (next:visited) next es
              Just (visited', c:cs)

data GRAph a = GRAph [a] [(a, a, Int)] deriving (Eq, Ord, Show)
graphPrim :: Eq a => GRAph a -> [(a, a, Int)] -- they want this instead of a Tree?
graphPrim (GRAph (n:nodes) weights) = go [n] nodes (map (\(x, y, z) -> ((x, y), z)) weights) []
  where
    go :: Eq a => [a] -> [a] -> [((a, a), Int)] -> [(a, a, Int)] -> [(a, a, Int)]
    -- nodes in tree, nodes not in tree, weights, and tree itself
    go _ [] _ x = x
    go a b e t = go (r:a) (r `delete` b) e ((mx, my, mc):t)
      where
        ((mx, my), mc) = minimumBy (comparing snd) (
          [n | n@((x, y), _) <- e, x `elem` a, y `elem` b] ++
          [n | n@((x, y), _) <- e, x `elem` b, y `elem` a])
        r = if mx `elem` b then mx else my

graphIso :: Ord a => Graph a -> Graph a -> Bool
graphIso (Graph a c) (Graph b d)
  | length a /= length b = False
  | length c /= length d = False
graphIso (Graph a b) (Graph c d) = [] /= [x | Just x <- map repl poss, ssort x == ssort d]
  where
    poss = map (zip a) (permutations c)
    repl x = mapM (\(i, j) -> do
      i' <- lookup i x
      j' <- lookup j x
      Just (i', j')
      ) b
    ssort = sortOn fst . sortOn snd -- radix sort

degree :: Eq a => Graph a -> a -> [a]
degree (Graph _ x) = gadj x
descDeg :: Eq a => Graph a -> [a]
descDeg (Graph x y) = sortOn (negate . length . gadj y) x
kColor :: forall a. Eq a => Graph a -> [(a, Int)]
kColor z@(Graph _ n) = go (descDeg z) 0 []
  where
    go :: Eq a => [a] -> Int -> [(a, Int)] -> [(a, Int)]
    go [] _ x = x
    go (x:xs) c t = go xs c' ((x, c'):t)
      where
        j = mapMaybe (`lookup` t) (gadj n x)
        c' = succ (maximum ((-1):j))

depthFirst :: forall a. Eq a => GRaph a -> a -> [a]
depthFirst (GRaph e) s = go [s] [] -- semi-ai
  where
    go :: Eq a => [a] -> [a] -> [a]
    go [] visited = reverse visited
    go (v:vs) visited
      | v `elem` visited = go vs visited
      | otherwise = go (filter (`notElem` (v:visited)) (fromMaybe [] (lookup v e)) ++ vs) (v:visited)

connComp :: Eq a => GRaph a -> [[a]]
connComp g@(GRaph e) = go (map fst e)
  where
    go [] = []
    go (x:xs) = let a = depthFirst g x in a:go (xs \\ a)

bipartite :: Eq a => Graph a -> Bool
bipartite = (< 2) . maximum . map snd . kColor
-- 90-94
queens :: Int -> [[Int]]
queens n = filter test (permutations [1..n])
  where
    test x = isNothing $ find (\(i, j) -> abs (i - j) == abs (x !! pred i - x !! pred j)) [(i, j) | i <- [1..n], j <- [1..n], i /= j]

knights :: Int -> [[(Int, Int)]]
knights n = go [] (1, 1)
  where
    go :: [(Int, Int)] -> (Int, Int) -> [[(Int, Int)]]
    go x n = let
        p = n:x
        next = filter (`notElem` p) (move n)
      in if null next
         then [reverse p]
         else concatMap (go p) next
    move :: (Int, Int) -> [(Int, Int)]
    move (x, y) = [(x', y') | dx <- [-2, -1, 1, 2], dy <- [-2, -1, 1, 2], abs dx /= abs dy,
      let (x', y') = (x + dx, y + dy), x' > 0, x' <= n, y' > 0, y' <= n]

vonKoch :: Graph Int -> [[Int]] -- namely nodes
-- it is possible to give 1-n to nodes and 1-(n-1) to edges such that
-- the value of an edge is the difference of the values of its nodes.
vonKoch (Graph x y) | pred (length x) /= length y = []
vonKoch (Graph nodes e) = catMaybes [test a b | a <- permutations [1..length nodes],
  b <- permutations [1..pred (length nodes)]]
  where
    repl x = mapM (\(i, j) -> do
      i' <- lookup i x
      j' <- lookup j x
      Just (i', j'))
    test :: [Int] -> [Int] -> Maybe [Int]
    test a b = do
      let a' = zip nodes a
      b' <- repl a' e
      let c = sort (map (\(i, j) -> abs (i - j)) b')
      if c == [1..pred (length nodes)] then Just a else Nothing

data Math = N Int | Plus Math Math | Minus Math Math | Mult Math Math | Div Math Math deriving (Eq, Show)
arith :: [Int] -> [(Math, Math)]
-- e.g. [0,2,3,5] -> 0 + 2 + 3 = 5 and 0 - 2 = 3 - 5 but NOT 3 + 2 = 5 + 0
arith nums = [(a', b') | (a, b) <- q nums, a' <- poss a, b' <- poss b, eval a' == eval b']
  where
    q x = (init . tail) (zip (inits x) (tails x))
    poss :: [Int] -> [Math]
    poss [x] = [N x]
    poss x = [f a' b' | (a, b) <- q x, a' <- poss a, b' <- poss b, f <- [Plus, Minus, Mult, Div]]
    eval :: Math -> Rational
    eval (N x) = fromIntegral x
    eval (Plus a b) = eval a + eval b
    eval (Minus a b) = eval a - eval b
    eval (Mult a b) = eval a * eval b
    eval (Div a b) = eval a / eval b

regular :: Int -> Int -> [Graph Int]
regular k n | n == 0 || k == 0 || n <= k || (odd n && odd k) = []
regular k n = nubBy graphIso [Graph [1..n] x | x <- poss, test x]
  where
    poss :: [[(Int, Int)]] -- list of edge-lists
    poss = [[(x !! i, x !! j) | (i, j) <- [(a, b) | a <- [0..pred n], b <- [0..pred n], a /= b]] | x <- permutations [1..n]]
    test :: [(Int, Int)] -> Bool
    test x = all ((== k) . length . gadj x) (map fst x ++ map snd x)
-- 94-99
fullWords :: Int -> String
fullWords x = intercalate "-" [fromJust (lookup i
  (zip "0123456789" ["zero","one","two","three","four","five","six","seven","eight","nine"])) | i <- show x] 

parse :: String -> Bool
parse = isJust . parse'
  where
    parse' x = do
      (_, y) <- letter x
      case y of
        "" -> Just ()
        _ -> loop y
    loop x = do
      (_, a) <- any' [hyphen, cont] x
      (_, b) <- any' [letter, number] a
      case b of
        "" -> Just ()
        _ -> loop b
    build f x = uncons x >>= (\y -> if f (fst y) then Just y else Nothing)
    any' f x = join (listToMaybe (map ($ x) f))
    letter = build isLowerCase
    number = build isDigit
    hyphen = build (== '-')
    cont = Just . (' ',)



---------------------------------------------------

test = "abcdefghi"
test2 = "aaabccddaddee"
test3 = ["ax", "bx", "cx", "defg", "h", "j", "lmn"]
test4 = 4268880 -- 66389621760
test5 = constTRee [5, 3, 18, 1, 4, 12, 21]
test6 = Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]
test7 = Graph [1..6] [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
test8 = GRAph [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
test9 = graphIso
  (Graph [1..8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)])
  (Graph [1..8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)])
test10 = Graph [1..7] [(1,4),(2,4),(3,4),(5,6),(6,3),(7,3)]


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
  --{-
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
  print . parseTReeS $ parseSTRee "a(b(d,e),c(,f(g,)))" -- note: exercise wants Maybe (TRee String), where Nothing is for invalid input
  print (preorder test5, inorder test5)
  print $ constPreIn (preorder test5) (inorder test5)
  print . parseTReeD $ parseDTRee "abd..e..c.fg..."
  print $ nnodes test6
  print . parseTreeS $ parseSTree "afg^^c^bd^e^^^"
  print $ ipl test6
  print $ bottomUp test6
  print $ displayL test6
  print $ adjToGraph (graphToAdj test7 True) True
  print . graphPaths 1 4 $ graphToAdj test7 False
  print . graphCycle 2 $ graphToAdj test7 False
  putStr . drawForest $ (map . fmap) show (graphTrees test7)
  print $ graphPrim test8
  print test9
  print $ kColor test7
  print $ depthFirst (graphToAdj test7 True) 1
  print $ connComp (graphToAdj test7 True)
  print $ queens 8 !! 1
  -- print $ vonKoch test10 !! 16 -- slow
  -- print $ arith [2, 3, 5, 7, 11] !! 6
  print $ regular 2 5 -- fix!
  print $ fullWords test4
  print $ parse "a1-b"
  --print $ 
  --print $ 
  --print $ 
  --}
