module PrologE where
import Prolog
import qualified Data.Map as M
import Data.List (intercalate)

instance Show Term where
  show (Var (x, [])) = x
  show (Var (x, t)) = x ++ show t
  show (Fun x []) = x
  show (Fun x y) = x ++ '(' : intercalate ", " (map show y) ++ ")"
instance Show Rule where
  show (x :- y) = show x ++ " :- " ++ intercalate ", " (map show y) ++ "."
showEnv :: Env -> String
showEnv x = unlines ("YES":[v ++ " <- " ++ show (walk x (Var s)) | (s@(v, []), _) <- M.toList x])
page :: [String] -> IO ()
page = foldr (\x -> (putStrLn x >> getLine >>)) (putStrLn "NO")

-- Read instances

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
