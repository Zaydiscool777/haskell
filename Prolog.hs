module Prolog (
  Sym, Term, Rule, Env, Res,
  matches, unify, solve,
  (-?>)) where
-- https://hackage.haskell.org/package/NanoProlog-0.3
import qualified Data.Map as M
import Data.Foldable (foldrM)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.List (intercalate, concatMap)

type Sym = String
data Term = Var Sym | Fun Sym [Term] deriving Eq
data Rule = Term :- [Term] deriving Eq
type Env = M.Map Sym Term
data Res = Yes Env | Do [(Rule, Res)] deriving Eq

class Subst t where
  subst :: Env -> t -> t
instance Subst Term where
  subst :: Env -> Term -> Term
  subst e v@(Var x) = maybe v (subst e) (M.lookup x e)
  subst e (Fun x cs) = Fun x (map (subst e) cs)
instance Subst Rule where
  subst :: Env -> Rule -> Rule
  subst e (c :- cs) = subst e c :- map (subst e) cs

(-?>) :: Sym -> Term -> Bool
x -?> (Var y) = x /= y
x -?> (Fun _ y) = all (x -?>) y

-- todo for matches and unify:
  -- add cycle checks (e.g. X = f(X)) by including an occurs-check
  -- have M.insert not overwrite previous bindings (reintroduce tag system?)

matches :: (Term, Term) -> Env -> Maybe Env -- for matching against rule head
matches (t, u) e = subst e t ? u
  where
    (?) :: Term -> Term -> Maybe Env
    Var x ? y | x -?> y = Just (M.insert x y e)
    Fun x xc ? Fun y yc
      | x == y && length xc == length yc
      = foldrM matches e (zip xc yc)
    _ ? _ = Nothing

unify :: (Term, Term) -> Env -> Maybe Env
unify (t, u) e = subst e t ?+ subst e u
  where
    (?+) :: Term -> Term -> Maybe Env
    Var x ?+ y | x -?> y = Just (M.insert x y e)
    x ?+ Var y | y -?> x = Just (M.insert y x e)
    Fun x xc ?+ Fun y yc
      | x == y && length xc == length yc
      = foldrM unify e (zip xc yc)
    _ ?+ _ = Nothing

solve :: [Rule] -> [Term] -> Env -> Res
solve _ [] e = Yes e
solve rs (t:ts) e = Do (catMaybes 
  [(r,) . solve rs (cs ++ ts) <$> unify (t, c) e | r@(c :- cs) <- rs])

strip :: Res -> Maybe Res
strip y@(Yes _) = Just y
strip (Do xs) = case mapMaybe (strip . snd) xs of
  [] -> Nothing
  ys -> Just (Do [(r, y) | (r, y) <- xs, y `elem` ys])

instance Show Term where
  show (Var x) = x
  show (Fun x y) = x ++ '(' : intercalate ", " (map show y) ++ ")"
instance Show Rule where
  show (x :- y) = show x ++ " :- " ++ intercalate ", " (map show y) ++ ".\n"
instance Show Res where
  show (Yes x) = "YES: " ++ show x
  show (Do x) = concatMap (\(d :- _, r) ->
    show d ++ " -> {" ++ show r ++ "}, ") x ++ "NO"

sym x = Fun x []
fact x y = Fun x y :- []
rules = [
  fact "edge" [sym "a", sym "b"],
  fact "edge" [sym "b", sym "c"],
  Fun "path" [Var "X", Var "Y"] :- [Fun "edge" [Var "X", Var "Y"]],
  Fun "path" [Var "X", Var "Y"] :- [Fun "edge" [Var "X", Var "Z"], Fun "path" [Var "Z", Var "Y"]]
  ]
goal = Fun "path" [sym "a", sym "c"]
solution = strip (solve rules [goal] M.empty)
main = print (fromMaybe (Do []) solution)
