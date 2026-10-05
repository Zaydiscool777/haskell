{-# LANGUAGE PatternSynonyms #-}
import Data.Bool (bool)
data Cons = A String | C Cons Cons
pattern Nil = A ""
pattern T = A "t"

-- quote
quote = id

-- atom
atom (A _) = T
atom _ = Nil

-- eq
instance Eq Cons where
  A x == A y = x == y
  _ == _ = False
eq x y = bool Nil T (x == y)

-- car
car (C x _) = x

-- cdr
cdr (C _ x) = x

-- cons
cons = C

-- cond
cond (C (C T x) _) = x
cond (C _ x) = cond x

-- funcs
append Nil = id
append (C x y) = C x . append y

pair Nil _ = Nil
pair (A x) y = (C (A x) y)
pair _ (A _) = Nil
pair (C x xs) (C y ys) = C (C x y) (pair xs ys)

assoc (C (C x y) z) w
  | w == x = y
  | otherwise = assoc z w

-- eval
eval :: Cons -> Cons -> Cons
eval a (C (A t) (C e Nil)) = case t of
  "quote" -> e
  "atom" -> atom (eval a e)
  "car" -> car (eval a e)
  "cdr" -> cdr (eval a e)
eval a (C (A t) (C e (C e' Nil))) = case t of
  "eq" -> eq (eval a e) (eval a e')
  "cons" -> cons (eval a e) (eval a e')
eval a (C (A "cond") e) = cond e
  where
    cond (C (C y x) z)
      | eval a y == T = eval a x
      | otherwise = cond z
eval a (C (C (A "label") (C (A f) e)) i) = eval (C (C (A f) e) a) (C e i)
eval a (C (C (A "lambda") (C g e)) i) = eval (append (pair g (evlis a i)) a) e
  where
    evlis _ Nil = Nil
    evlis a (C x xs) = C (eval a x) (evlis a xs)

-- repl
instance Show Cons where
  show (A x) = x
  show (C x y) = '(' : showl y
    where
      showl Nil = ")"
      showl (C x Nil) = show x ++ ")"
      showl (C x (A y)) = show x ++ " . " ++ y ++ ")"
      showl (C x xs) = show x ++ ' ' : showl xs

main = do
  print . eval Nil $ C (A "quote") (C (A "hello") Nil) 
