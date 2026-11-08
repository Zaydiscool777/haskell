-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE Rank2Types #-}
module Hout.HoutEx where

import Data.Kind
import Data.Proxy
import Data.Type.Equality

-- Witness (a :: k) ~ Proxy (a :: k)

-- replace with Generic?
data Exists k (p :: k -> Type) where
  Exists :: Proxy (a :: k) -> p a -> Exists k p

-- Despite forall being a Haskell built-in, the lack of
-- support for impredicative types means that a newtype is declared for
-- usability.
newtype Forall k (p :: k -> Type) = Forall (forall (a :: k). p a)

-- Equal (a :: k) (b :: k) ~ (a :: k) :~: (b :: k)

existsIntro :: Proxy (a :: k) -> p a -> Exists k p
existsIntro = Exists

-- Note that this has to be expressed in this continuation style because
-- existential types are not allowed in the return type
existsElim :: (forall (a :: k). Proxy a -> p a -> b) -> Exists k p -> b
existsElim f (Exists witness proof) = f witness proof

forallIntro :: (forall (a :: k). p a) -> Forall k p
forallIntro = Forall

forallElim :: Forall k p -> p (a :: k)
forallElim (Forall forall_) = forall_
