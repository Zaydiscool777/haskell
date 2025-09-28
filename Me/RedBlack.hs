import Data.Maybe
import Debug.Trace

data Tree a =
  Nil {par :: Maybe (Tree a)} |
  Node {
    valP :: a,
    colorP :: Bool,
    left :: Tree a,
    right :: Tree a,
    par :: Maybe (Tree a)}

instance (Show a) => Show (Tree a) where
  show :: (Show a) => Tree a -> String
  show = unlines . draw
    where
      draw :: (Show a) => Tree a -> [String]
      draw n | isNil n = ["*Nil"]
      draw a
        | all (isNil . ($ a)) [left, right] =
          [color ++ "leaf " ++ show (val a)]
        | otherwise = (init:) $ sp2 : dleft ++ sp : dright
          where
            init = color ++ show (val a)
            color = if isBlack a then "/BLACK " else "/RED "
            dleft = map ("< "++) $ draw $ left a
            dright = map ("> "++) $ draw $ right a
            sp = "├"; sp2 = "|"

isNil :: Tree a -> Bool
isNil (Nil _) = True
isNil (Node {}) = False

val :: Show a => Tree a -> a
val n | isNil n = error ("Nil has no value: " ++ show (parF n))
val a = valP a

isBlack :: Tree a -> Bool
isBlack n | isNil n = True
isBlack x = colorP x

isRed :: Tree a -> Bool
isRed = not . isBlack

parF :: Show a => Tree a -> Tree a
parF = fromJust . par
parF2 :: Show a => String -> Tree a -> Tree a
parF2 y = fromJust . par --(trace ("parF " ++ y ++ ' ' : show (par x) ++ ' ' : show x) x)

upar :: Show a => Tree a -> Maybe (Tree a) -> Tree a
upar n p | isNil n = Nil p
upar a p =
  Node (val a) (isBlack a)
    (upar (left a) (Just a))
    (upar (right a) (Just a)) p

uPar :: Show a => Tree a -> Tree a
uPar x = upar x (par x)

nodeR :: Show a => a -> Bool -> Tree a -> Tree a -> Tree a
nodeR v c l r = uPar (Node v c l r Nothing)

nilR :: Tree a
nilR = Nil Nothing

isRoot :: Tree a -> Bool
isRoot = isNothing . par

leaf :: Show a => a -> Tree a
leaf x = nodeR x False nilR nilR

onLeft :: (Eq a, Show a) => Tree a -> Bool
onLeft a = (val . left . parF2 "onLeft") a == val a

onRight :: (Eq a, Show a) => Tree a -> Bool
onRight = not . onLeft

ninsert :: (Eq a, Ord a, Show a) => Tree a -> a -> Tree a -- returns the given node changed
ninsert a v
  | isNil a = (leaf v) {par = Nothing}
  | v > val a = uPar a {right = ninsert (right a) v}
  | otherwise = uPar a {left = ninsert (left a) v}

ninsert' :: (Eq a, Ord a, Show a) => Tree a -> a -> Tree a -- returs the node where it was inserted
ninsert' a v
  | isNil a = (leaf v) {par = Nothing}
  | v > val a = right (uPar a {right = ninsert' (right a) v})
  | otherwise = left (uPar a {left = ninsert' (left a) v})

gran :: Show a => Tree a -> Tree a
gran = parF2 "gran2" . parF2 "gran1"

unc :: (Eq a, Show a) => Tree a -> Tree a
unc x = ((if onRight (parF2 "unc-if" x) then left else right) . parF2 "unc-get") x

rotR :: Show a => Tree a -> Tree a
rotR a = uPar (b {right = a {left = nilR}}) where b = left a

rotL :: Show a => Tree a -> Tree a
rotL a = uPar (b {left = a {right = nilR}}) where b = right a

search :: (Eq a, Ord a, Show a) => Tree a -> a -> Maybe (Tree a)
search a v
  | isNil a = Nothing
  | val a == v = Just a
  | v > val a = search (right a) v
  | otherwise = search (left a) v

isInner :: (Eq a, Show a) => Tree a -> Bool
isInner x = onLeft x /= onLeft (parF2 "isInner" x)

isOuter :: (Eq a, Show a) => Tree a -> Bool
isOuter = not . isInner

root :: Tree a -> Tree a
root x
  | isNothing (par x) = x
  | otherwise = fromJust (par x)

{-b arrow=aft- g
 / \ er rot.  / \
d   a->    <-p   u
     \      /
      c    n-}

insert :: (Eq a, Ord a, Show a) => Tree a -> a -> Tree a
insert a v = insert' (ninsert' a v)
  where
    insert' :: (Eq a, Show a) => Tree a -> Tree a -- Ord a?
    insert' a
      | isRoot a = uPar a -- case 1
      | isBlack (parF2 "case3-if" a) = uPar a -- case 3
      | isRoot (parF2 "case4-if" a) {-&& isRed (parF a)-} = uPar $ a {par = Just (parF2 "case4-setcolor" a) {colorP = True}} -- case 4
    insert' a | {-isRed (parF a) &&-} isRed (unc a) = -- case 2
      uPar $ insert' (gran c) -- once we change colors, go to gran, since its par could be red
        where
          c = b {
            par = Just (parF2 "case2-seteasy" b) {colorP = True, -- make par black
              par = Just (gran b) {colorP = False} -- make gran red
            }
          }
          b = if onLeft (parF2 "case2-ifhard" a) -- this changes unc
            then
              a {par = Just (parF2 "case2-sethard-left" a) {par = Just (gran a) {
                    right = (right (gran a)) {colorP = True}
              }}}
            else
              a {par = Just (parF2 "case2-sethard-right" a) {par = Just (gran a) {
                    left = (left (gran a)) {colorP = False}
              }}}
    insert' a | {-isRed (parF a) &&-} isBlack (unc a) && isInner a = -- case 5
      uPar $ insert' (b a)
        where
          b = (if onLeft a
            then
              left . rotL -- since this is on the parent, we go to the node after rot.
            else
              right . rotR) . parF2 "case5"
    insert' a | {-isRed (parF a) &&-} isBlack (unc a) && isOuter a = -- case 6
      uPar $ insert' (b (gran a) {colorP = False}) {colorP = True} -- this is like case 2, but w/ rot.
        where b = if onLeft a then rotR else rotL-- since a is outer, this is equal to onLeft (parF a)

main :: IO ()
main = return ()
