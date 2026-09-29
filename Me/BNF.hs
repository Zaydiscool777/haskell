import Control.Applicative
import Data.List (intercalate)

type Rulename = String
type Rule = (Rulename, Expr)
type Syntax = [Rule]
data Expr =
  Lit Char -- 'a'
  | Join Expr Expr -- a, a, a...
  | Ref Rulename -- <a>
  | Union Expr Expr -- a | a
  | Empty -- ε
  deriving (Eq)

instance Show Expr where
  show :: Expr -> String
  show (Lit x) = ['\'', x, '\'']
  show (Join x y) = show x ++ ", " ++ show y
  show (Ref x) = '<' : x ++ ">"
  show Empty = "ε"
  show (Union x y) =
    '(' : show x ++ " | " ++ show y ++ ")"

data Match =
  Ok {logs :: [Res], text :: String}
  | No {logs :: [Res]}

instance Show Match where
  show :: Match -> String
  show (Ok lg t) = "Ok " ++ show t
  show (No lg) = "No {" ++ show lg ++ "}"

instance Semigroup Match where
  (<>) :: Match -> Match -> Match
  No lg1 <> No lg2 = No (lg1 ++ lg2)
  No lg1 <> Ok lg2 t = Ok (lg1 ++ lg2) t
  Ok lg1 t <> x = Ok (lg1 ++ logs x) t

instance Monoid Match where
  mempty :: Match
  mempty = No []

data Res =
  RLit Char
  | RJoin Res Res
  | RRef Rulename [Res]
  | RLeft [Res]
  | RRight [Res]
  | REmpty
  | RFail Char Char -- match (Lit e) a -> RFail e a

instance Show Res where
  show :: Res -> String; show = show' where
    nicec :: (Show a) => [a] -> String
    nicec x = '(' : intercalate ", " (map show x) ++ ")"
    show' (RLit c) = ['\'', c, '\'']
    show' (RJoin a b) = show a ++ " ++ " ++ show b
    show' (RRef n rs) = '<' : n ++ '>' : nicec rs
    show' (RLeft rs) = "←" ++ nicec rs
    show' (RRight rs) = "→" ++ nicec rs
    show' REmpty = "ε"
    show' (RFail e a) = e : " != " ++ [a]

upd :: Match -> Res -> Match
upd (Ok lg x) l = Ok (l:lg) x
upd (No lg) l = No (l:lg)
updb :: Match -> [Res] -> Match
updb (Ok lg x) l = Ok (lg++l) x
updb (No lg) l = No (lg++l)
delog :: Match -> Match
delog x = x {logs = []}

match' :: Syntax -> Expr -> Match -> Match
match' _ _ (No lg) = No lg
match' _ (Lit _) (Ok lg "") = No lg
match' _ (Lit c) (Ok lg (t:ts))
  | c == t = Ok (RLit t:lg) ts
  | otherwise = No (RFail c t:lg)
match' syn (Join a b) (Ok lg s) =
  updb buff2 (logs buff1)
  where
    ms = match' syn
    buff1 = ms a (Ok lg s)
    buff2 = ms b (buff1 {logs = []})
match' syn (Ref name) (Ok lg s) = buff
  where
    ms = match' syn
    ff = lookup name syn
    buff = case ff of
      Nothing -> Ok lg s -- fail?
      Just f -> res {logs = [RRef name (logs res)]}
        where res = ms f (Ok [] s)
match' syn (Union a b) (Ok lg s) = ms res (Ok [addl] s)
  where
    ms = match' syn
    buff1 = ms a (Ok [] s)
    (res, addl) = case buff1 of
      Ok _ _ -> (a, RLeft (logs buff1))
      No _ -> (b, RRight (logs buff1))
      -- union is right lazy: left supersedes right, and only right can fail
match' _ Empty (Ok lg s) = Ok (REmpty:lg) s

match :: Syntax -> String -> Match
match s x = match' s (Ref "main") (Ok [] x)

matchm :: Syntax -> Expr -> String -> Match
matchm s e x = match' s e (Ok [] x)

infixl 5 =:
(=:) :: Rulename -> Expr -> Rule
(=:) = (,)
infixl 7 //
(//) :: Expr -> Expr -> Expr
(//) = Union
infixr 8 ./ -- ++ is slow
(./) :: Expr -> Expr -> Expr
x ./ (Join a b) = Join x (Join a b)
(Join a b) ./ y = Join a (Join b y)
x ./ y = Join x y
strlit :: String -> Expr
strlit = foldr1 Join . map Lit
-- nothing = Join []
-- orlit :: String -> Expr
orlit :: [Char] -> Expr
orlit = foldr1 Union . map Lit

x :: Match
x = match [
    "a" =: Lit 'a',
    "b" =: Lit 'b' ./ Ref "b" // Empty,
    "main" =: Ref "a" // Ref "b"
  ] "bb"

main :: IO ()
main = print x
