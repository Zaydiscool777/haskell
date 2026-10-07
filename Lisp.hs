{-# LANGUAGE PatternSynonyms, BlockArguments #-}
import Control.Monad (join)
import System.Console.Readline (readline)
--import Debug.Trace

data Cons = A String | C Cons Cons
pattern Nil = A ""
pattern T = A "t"

quote = Just

atom (A _) = Just T
atom _ = Just Nil

instance Eq Cons where
  A x == A y = x == y
  _ == _ = False
eq x y = Just if x == y then T else Nil

car (C x _) = Just x
car _ = Nothing

cdr (C _ x) = Just x
cdr _ = Nothing

cons = (Just .) . C

cond (C (C T x) _) = Just x
cond (C _ x) = cond x
cond _ = Nothing

-- funcs
append Nil = Just
append (C x y) = (C x <$>) . append y
append _ = const Nothing

pair Nil _ = Just Nil
pair (A x) y = Just (C (A x) y)
pair _ (A _) = Just Nil
pair (C x xs) (C y ys) = C (C x y) <$> pair xs ys

assoc (C (C x y) z) w
  | w == x = Just y
  | otherwise = assoc z w
assoc _ _ = Nothing

-- eval
eval :: Cons -> Cons -> Maybe Cons
eval a e@(A _) = assoc a e
eval a (C (A t) (C e Nil)) = case t of
  "quote" -> quote e
  "atom" -> atom =<< e'
  "car" -> car =<< e'
  "cdr" -> cdr =<< e'
  where e' = eval a e
eval a (C (A t) (C e (C e2 Nil))) = case t of
  "eq" -> join (eq <$> e' <*> e2')
  "cons" -> join (cons <$> e' <*> e2')
  where e' = eval a e; e2' = eval a e2
eval a (C (A "cond") e) = evcon e
  where
    evcon (C (C y x) z)
      | eval a y == Just T = eval a x
      | otherwise = cond z
eval a (C f@(A _) e) = assoc a f >>= flip cons e >>= eval a
eval a (C (C (A "label") e'@(C (A _) e)) i) = eval (C e' a) (C e i)
eval a (C (C (A "lambda") (C g e)) i) = let
    evlis _ Nil = Just Nil
    evlis a (C x xs) = join (cons <$> eval a x <*> evlis a xs)
  in evlis a i
  >>= pair g
  >>= flip append a
  >>= flip eval e
eval _ _ = Nothing

-- repl
instance Show Cons where
  show Nil = "()"
  show (A x) = x
  show (C x y) = '(' : showl (C x y)
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

main = (readline "> " >>= maybe (putStrLn "x") print . (>>= (eval Nil . read))) >> main

-- ideas:
-- have eval serve a so dictionary can be used over expressions
-- add numbers, string, etc.
-- add quote prefix: 'a
-- add IO?
