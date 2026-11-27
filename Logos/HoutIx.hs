{-# LANGUAGE RebindableSyntax, BlockArguments #-}
module Logos.HoutIx where
import Prelude ((.), ($), const)
import qualified Prelude as P

class Functor f where fmap :: f j k a -> (a -> b) -> f j k b
class Functor m => Applicative m where
  pure :: a -> m i i a
  (<*>) :: m i j (a -> b) -> m j k a -> m i k b
class Applicative m => Monad m where
  return :: a -> m i i a
  return = pure
  (>>=) :: m i j a -> (a -> m j k b) -> m i k b
  (>>) :: m i j a -> m j k b -> m i k b
  f >> g = f >>= P.const g

newtype Tactic from to a = Tactic {act :: (a -> to) -> from}

instance Functor Tactic where -- apply tactic
  fmap (Tactic g) proof = Tactic \f -> g (f . proof)
instance Applicative Tactic where -- normal modus ponens
  pure a = Tactic ($ a)
  (Tactic g) <*> (Tactic h) = Tactic \f -> g \a_b -> h (f . a_b)
instance Monad Tactic where -- functor-like modus ponens
  (Tactic g) >>= f = Tactic \b_o -> g \a -> act (f a) b_o
  (Tactic g) >> (Tactic f) = Tactic (g . const . f)

{-
Tactic to from a ~ from implies to (and you have a)
z = do
  x -- Tactic a b _
  y -- Tactic b c _
z :: Tactic a c
-}
