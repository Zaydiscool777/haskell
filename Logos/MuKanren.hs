{-# LANGUAGE ViewPatterns, BlockArguments #-}
module Logos.MuKanren where
-- http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
-- it is actually called microkanren
import Control.Monad.State
import Control.Monad
import Control.Applicative
import Data.Maybe (maybeToList)

type Var = Integer
data Term a = Atom a | Var Var | Join (Term a) (Term a)

type Subs a = [(Var, Term a)]
-- assp = lookup but also returns orig. a -> [(a, b)] -> Maybe (a, b)
type Bind a = (Subs a, Var)
type Goal a = StateT (Bind a) [] ()
-- ReaderT (Subs a) (StateT Var []) (Subs a)?

eq :: Eq a => Term a -> Term a -> Goal a
eq u v = StateT \(s, c) -> case unify s u v of
  Nothing -> empty
  Just x -> pure (pure (x, c))

walk :: Subs a -> Term a -> Term a
walk s u@(Var v) = case lookup v s of
  Nothing -> u
  Just x -> walk s x

unify :: Eq a => Subs a -> Term a -> Term a -> Maybe (Subs a)
unify s (walk s -> Var u) (walk s -> Var v) | u == v = Just s
unify s (walk s -> Var u) (walk s -> v) = Just ((u, v):s)
unify s (walk s -> u) (walk s -> Var v) = Just ((v, u):s)
unify s (walk s -> Join u u') (walk s -> Join v v') = do
  s' <- unify s u v
  unify s' u' v'
unify s (walk s -> Atom u) (walk s -> Atom v) | u == v = Just s
unify _ _ _ = Nothing

callFresh :: (Term a -> Goal a) -> Goal a
callFresh f = do
  (s, c) <- get
  put (s, succ c)
  f (Var c)
