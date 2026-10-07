-- module Prolog where
-- https://hackage.haskell.org/package/NanoProlog-0.3
import Data.Map (Map)
import qualified Data.Map as M
import Data.Foldable (foldrM)
import Data.Maybe (catMaybes)
import Data.List (intercalate, concatMap)

type Sym = String
data Term = Var Sym | Fun Sym [Term]
data Rule = Term :- [Term]
type Env = Map Sym Term
data Res = Yes Env | Do [(Rule, Res)]

class Subst t where
  subst :: Env -> t -> t
instance Subst Term where
  subst :: Env -> Term -> Term
  subst e v@(Var x) = case M.lookup x e of
    Just j -> subst e j
    Nothing -> v
  subst e (Fun x cs) = Fun x (map (subst e) cs)
instance Subst Rule where
  subst :: Env -> Rule -> Rule
  subst e (c :- cs) = subst e c :- map (subst e) cs

matches :: (Term, Term) -> Env -> Maybe Env
matches (t, u) e = subst e t ? u
  where
    (?) :: Term -> Term -> Maybe Env
    Var x ? y = Just (M.insert x y e)
    Fun x xc ? Fun y yc
      | x == y && length xc == length yc
      = foldrM matches e (zip xc yc)
    _ ? _ = Nothing

unify :: (Term, Term) -> Env -> Maybe Env
unify (t, u) e = subst e t ?+ subst e u
  where
    (?+) :: Term -> Term -> Maybe Env
    Var x ?+ y = Just (M.insert x y e)
    x ?+ Var y = Just (M.insert y x e)
    Fun x xc ?+ Fun y yc
      | x == y && length xc == length yc
      = foldrM unify e (zip xc yc)

solve :: [Rule] -> [Term] -> Env -> Res
solve _ [] e = Yes e
solve rs (t:ts) e = Do (catMaybes 
  [(r,) . solve rs (cs ++ ts) <$> unify (t, c) e | r@(c :- cs) <- rs])

instance Show Term where
  show (Var x) = x
  show (Fun x y) = x ++ '(' : intercalate ", " (map show y) ++ ")"
instance Show Rule where
  show (x :- y) = show x ++ " :- " ++ intercalate ", " (map show y) ++ ".\n"
instance Show Res where
  show (Yes x) = "YES: " ++ show x
  show (Do x) = concatMap (\(d :- _, r) ->
    "using " ++ show d ++ " -> " ++ show r) x ++ "end"

main = return ()
