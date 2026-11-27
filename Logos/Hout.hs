{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
module Logos.Hout (apply, intro, exact, assert, enough, deply, runTheorem, qed) where
-- https://hackage.haskell.org/package/hout
import Logos.HoutEx
import Logos.HoutIx (Tactic(..))
import qualified Logos.HoutIx as Ix
import Data.Void -- False/Axiom
-- import Data.Proxy -- Existence
-- import Data.Type.Equality -- Equal

-- true | ()
-- false | Void
-- and | (,)
-- or | Either
-- not | -> Void

-- tactics

apply :: (a -> b) -> Tactic b a ()
apply f = Tactic $ f . ($ ())
-- e.g. left :: Tactic (a \/ b) a () = apply Left

intro :: Tactic (a -> b) b a
intro = Tactic id

exact :: a -> Tactic a () ()
exact = Tactic . const

assert :: Tactic a () () -> Tactic m m a
assert (Tactic f) = Tactic ($ f id)

enough :: (a -> Tactic b () ()) -> Tactic b a () -- monadic apply
enough t = Tactic \f -> let Tactic g = t (f ()) in g id

deply :: Tactic b a () -> a -> b
deply (Tactic f) = f . const
-- :: Iso (a -> b) (Tactic b a ()) = iso apply deply

-- theorem actions
type Theorem a = Tactic a () ()
runTheorem :: Theorem a -> a
runTheorem (Tactic f) = f id

qed :: Tactic () () ()
qed = exact ()

-- example

orComm :: Theorem (Either a b -> Either b a)
orComm = Ix.do
  a <- intro -- Tactic (Either a b -> Either b a) (Either b a) (Either a b)
  case a of -- Tactic (Either b a) () ()
    Left b -> Ix.do
      apply Right -- (a -> Either b a) -> Tactic (Either b a) a ()
      exact b -- a -> Tactic a () ()
    Right b -> Ix.do
      apply Left -- (b -> Either b a) -> Tactic (Either b a) b ()
      exact b -- b -> Tactic b () ()
  qed -- Tactic () () ()
