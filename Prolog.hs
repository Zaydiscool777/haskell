module Prolog (
  Sym(..), Term(..), Rule(..), Env(..), Res(..), Tagged(..),
  subst, (-?>), unify, solve,
  matches, strip, walk,
  sym, fact, var, query) where -- https://hackage.haskell.org/package/NanoProlog-0.3
import qualified Data.Map as M
import Data.Foldable (foldrM)
import Data.Maybe (catMaybes)
import Data.List (nubBy)
import Data.Function (on) -- why no nubOn in base!

type Sym = String
type Tag = Int
type VarD = (Sym, [Tag]) -- Var data
data Term = Var VarD | Fun Sym [Term] deriving Eq
data Rule = Term :- [Term] deriving Eq
type Env = M.Map VarD Term
data Res = Yes Env | Do [(Rule, Res)] deriving Eq

subst :: Env -> Term -> Term -- substitiution
subst e v@(Var x) = maybe v (subst e) (M.lookup x e)
subst e (Fun x cs) = Fun x (map (subst e) cs)

class Tagged t where tag :: Tag -> t -> t -- variable renaming apart
instance Tagged Term where
  tag t (Var (x, y)) = Var (x, t:y)
  tag t (Fun x y) = Fun x (map (tag t) y)
instance Tagged Rule where tag t (c :- cs) = tag t c :- map (tag t) cs

(-?>) :: VarD -> Term -> Bool -- occurs-check (negated)
x -?> Var y = x /= y
x -?> Fun _ y = all (x -?>) y

matches :: (Term, Term) -> Env -> Maybe Env -- replacement / smaller check before unify
matches (t, u) e = subst e t ?- u where
  (?-) :: Term -> Term -> Maybe Env
  Var x ?- y | x -?> y = Just (M.insert x y e)
  Fun x xc ?- Fun y yc
    | x == y && length xc == length yc
    = foldrM matches e (zip xc yc)
  _ ?- _ = Nothing

unify :: (Term, Term) -> Env -> Maybe Env -- inference between terms
unify (t, u) e = subst e t ? subst e u where
  (?) :: Term -> Term -> Maybe Env
  Var x ? y | x -?> y = Just (M.insert x y e)
  x ? Var y | y -?> x = Just (M.insert y x e)
  Fun x xc ? Fun y yc
    | x == y && length xc == length yc
    = foldrM unify e (zip xc yc)
  _ ? _ = Nothing

solve :: [Rule] -> [Term] -> Tag -> Env -> Res -- the crux of prolog
solve _ [] _ e = Yes e
solve rs (t:ts) tg e = Do (catMaybes
  [(r,) . solve rs (cs ++ ts) (succ tg) <$> unify (t, c) e | r@(c :- cs) <- map (tag tg) rs])

strip :: Res -> [Env]
strip (Yes y) = [y]
strip (Do x) = nubBy ((==) `on` M.mapKeys fst) (x >>= strip . snd)

walk :: Env -> Term -> Term
walk e s@(Var _) = walk e (subst e s)
walk e s@(Fun _ _) = s

-- shorthands
var x = Var (x, [])
sym x = Fun x []
fact x y = Fun x y :- []
query x y = solve x [y] 0 M.empty
