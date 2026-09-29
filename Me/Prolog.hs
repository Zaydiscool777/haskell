module Prolog where
import Control.Monad
import Control.Monad.ST
import Data.STRef

a, b, c, d, e, f, x, y :: Term
a = Atom "a"
b = Atom "b"
c = Atom "c"
d = Atom "d"
e = Atom "e"
f = Atom "f"
x = Var "X"
y = Var "Y"

data Relation =
  Fact Term -- a.
  | Rule Term Terms -- a :- b, c.
  deriving (Eq, Show)
type Relations = [Relation]

data Term =
  Atom String -- a.
  | Var String -- X.
  | Comp String Terms -- a(b,c).
  deriving (Eq, Show)
type Terms = [Term]

data Res =
  Yes
  | No
  | Match Assum Ress Res -- match = cons, no = nil
  deriving (Eq, Show)
type Assum = (String, Term)
type Ress = [Res]

{-- -- this is not a good way to implement. a stack should be used instead and stuff
maymatch :: Res -> Bool
maymatch Yes = True
maymatch No = False
maymatch (Match _ a b) = any maymatch a || maymatch b

-- context puts one result in the context of the other: res1{yes -> res2}
context :: Res -> Res -> Res
context No _ = No
context Yes x = x
context (Match a b c) x = Match a (map (`context` x) b) (context c x)

-- respairs turns a result into a list of lists of pairs that return yes
respairs :: Res -> [[Assum]]
respairs No = [] -- there is no list of pairs that satisfy
respairs Yes = [[]] -- there is one, where nothing has to be assummed
respairs (Match a b c) = map (a:) (join (map respairs b)) ++ respairs c

-- resgiv checks if a term is true given one other term
resgiv :: Term -> Term -> Res
resgiv (Atom a) (Atom b) | a == b = Yes
resgiv x@(Atom a) (Var b) = Match (b, x) [Yes] No
resgiv x@(Atom a) _ = No
resgiv (Var a) b = Match (a, b) [Yes] No
resgiv x@(Comp a b) y@(Comp c d)
  | x == y = Yes
resgiv (Comp a (b:bs)) (Comp c (d:ds))
  | a == c && maymatch (resgiv b d) = context (resgiv b d) (resgiv (Comp a bs) (Comp c ds))
resgiv (Comp a (Var _:b)) (Comp c (Var _:d))
  | a == c = resgiv (Comp a b) (Comp c d)
resgiv (Comp a (Var b:bs)) (Comp c (d:ds))
  | a == c = Match (b, d) [resgiv (Comp a bs) (Comp c ds)] No
resgiv (Comp a (b:bs)) (Comp c (Var d:ds))
  | a == c = Match (d, b) [resgiv (Comp a bs) (Comp c ds)] No
resgiv _ _ = No

resgivs :: Term -> Terms -> Res
resgivs _ [] = Yes
resgivs x (y:ys) = context (resgiv x y) (resgivs x ys)

ressgivs :: Terms -> Terms -> Res
ressgivs [] _ = Yes
ressgivs (x:xs) ys = context (resgivs x ys) (ressgivs xs ys)

defLan :: Relation -> STRef s Relations -> ST s ()
defLan x s = readSTRef s >>= writeSTRef s . (x:)
--}
