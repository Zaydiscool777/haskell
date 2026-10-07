{-# LANGUAGE PatternSynonyms #-}
data Cons = A String | C Cons Cons
pattern Nil = A ""
pattern T = A "t"

quote = id

atom (A _) = T
atom _ = Nil

instance Eq Cons where
  A x == A y = x == y
  _ == _ = False
eq x y = if x == y then T else Nil

car (C x _) = x

cdr (C _ x) = x

cons = C

cond (C (C T x) _) = x
cond (C _ x) = cond x

-- funcs
append Nil = id
append (C x y) = C x . append y

pair Nil _ = Nil
pair (A x) y = C (A x) y
pair _ (A _) = Nil
pair (C x xs) (C y ys) = C (C x y) (pair xs ys)

assoc (C (C x y) z) w
  | w == x = y
  | otherwise = assoc z w

-- eval
eval :: Cons -> Cons -> Cons
eval a (C (A t) (C e Nil)) = case t of
  "quote" -> e
  "atom" -> atom e'
  "car" -> car e'
  "cdr" -> cdr e'
  where e' = eval a e
eval a (C (A t) (C e (C e2 Nil))) = case t of
  "eq" -> eq e' (eval a e2)
  "cons" -> cons e' (eval a e2)
  where e' = eval a e
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
  show (C x y) = '(' : show x ++ ' ' : showl y
    where
      showl Nil = ")"
      showl (C x Nil) = show x ++ ")"
      showl (C x (A y)) = show x ++ " . " ++ y ++ ")"
      showl (C x xs) = show x ++ ' ' : showl xs

instance Read Cons where
  readsPrec _ s =
    case f s of
      Left y -> [y]
      Right _ -> []
    where
      first f (x, y) = (f x, y)
      f :: String -> Either (Cons, String) String
      f ('(':x) = Left (first g (unfoldr f x))
        where
          unfoldr f x = case f x of
            Left (a, b) -> first (a:) (unfoldr f b)
            Right b -> ([], b)
          g x@(_:_:_) | last (init x) == A "."
            = foldr1 C (init (init x) ++ [last x])
          g x = foldr C Nil x
      f (')':x) = Right x
      f (' ':x) = f x
      f x = (Left . first A . break (`elem` "() ")) x

main = readLn >>= print . eval Nil >> main
