module PrologE where
import Prolog
import qualified Data.Map as M
import Data.Foldable (foldrM)
import Data.List (intercalate)

matches :: (Term, Term) -> Env -> Maybe Env -- looser unify?
matches (t, u) e = subst e t ?- u where
  (?-) :: Term -> Term -> Maybe Env
  Var x ?- y | x -?> y = Just (M.insert x y e)
  Fun x xc ?- Fun y yc
    | x == y && length xc == length yc
    = foldrM matches e (zip xc yc)
  _ ?- _ = Nothing

-- io
instance Show Term where
  show (Var (x, [])) = x
  show (Var (x, t)) = x ++ show t
  show (Fun x []) = x
  show (Fun x y) = x ++ '(' : intercalate ", " (map show y) ++ ")"
instance Show Rule where
  show (x :- y) = show x ++ " :- " ++ intercalate ", " (map show y) ++ "."
showEnv :: Env -> String
showEnv x = unlines ("YES":[v ++ " <- " ++ show (subst x (Var s)) | (s@(v, []), _) <- M.toList x])
page :: [String] -> IO ()
page = foldr (\x -> (putStr x >> getLine >>)) (putStrLn "NO") -- replaces: putStr . unlines

-- Read instances...are not easy without non-base libraries!
-- main
rules = [
  fact "edge" [sym "a", sym "b"],
  fact "edge" [sym "b", sym "c"],
  Fun "path" [var "X", var "Y"] :- [Fun "edge" [var "X", var "Y"]],
  Fun "path" [var "X", var "Y"] :- [Fun "edge" [var "X", var "Z"], Fun "path" [var "Z", var "Y"]]
  ]
goal = Fun "path" [sym "a", var "X"]
solution = strip (query rules goal)
main = page (map showEnv solution)
