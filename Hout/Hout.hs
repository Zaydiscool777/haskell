{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
module Hout.Hout (main) where
-- https://hackage.haskell.org/package/hout
import Hout.HoutEx
import Hout.HoutIx (Tactic(..))
import qualified Hout.HoutIx as Ix
import Data.Void -- False
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

idk :: Tactic b a () -> a -> b
idk (Tactic f) = f . const
-- the opposite of apply is exact . idk

-- theorem actions
type Theorem a = Tactic a () ()
runProof :: Theorem a -> a
runProof (Tactic f) = f id

qed :: Tactic () () ()
qed = exact ()

data Axiom a
admit :: forall {k} (a :: k). Axiom a
admit = admit

-- main
main :: Tactic () () ()
main = Ix.do
  qed

orComm :: Theorem (Either a b -> Either b a)
orComm = Ix.do
  a <- intro
  case a of
    Left b -> Ix.do
      apply Right
      exact b
    Right b -> Ix.do
      apply Left
      exact b
  qed
