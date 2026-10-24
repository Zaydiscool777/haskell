module Hout (main) where
-- https://hackage.haskell.org/package/hout
import HoutEx
import Data.Void
import Data.Proxy -- Witness
import Data.Type.Equality -- Equal

class IxFunctor f where imap :: f j k a -> (a -> b) -> f j k b
class IxFunctor m => IxPointed m where ireturn :: a -> m i i a
class IxPointed m => IxApplicative m where iap :: m i j (a -> b) -> m j k a -> m i k b
class IxApplicative m => IxMonad m where ibind :: m i j a -> (a -> m j k b) -> m i k b

newtype Ix m i j a = Ix {deIx :: m a}
instance Functor m => IxFunctor (Ix m) where imap (Ix ma) f = Ix (f <$> ma)
instance Applicative m => IxPointed (Ix m) where ireturn x = Ix (pure x)
instance Applicative m => IxApplicative (Ix m) where iap (Ix mf) (Ix ma) = Ix (mf <*> ma)
instance Monad m => IxMonad (Ix m) where ibind (Ix ma) f = Ix (ma >>= deIx . f)

instance IxFunctor Tactic where -- apply tactic
  imap :: Tactic m n a -> (a -> b) -> Tactic m n b
  imap (Tactic g) proof = Tactic (\f -> g (f . proof))
instance IxPointed Tactic where
  ireturn :: a -> Tactic b b a
  ireturn a = Tactic ($ a)
instance IxApplicative Tactic where -- normal modus ponens
  iap :: Tactic m n (a -> b) -> Tactic n o a -> Tactic m o b
  iap (Tactic g) (Tactic h) = Tactic (\f -> g (\a_b -> h (f . a_b)))
instance IxMonad Tactic where -- functor-like modus ponens
  ibind :: Tactic m n a -> (a -> Tactic n o b) -> Tactic m o b
  ibind (Tactic g) f = Tactic (\b_o -> g (\a -> act (f a) b_o))


-- true | ()
-- false | Void
-- and | (,)
-- or | Either
-- not | -> Void

-- tactics
data Tactic from to a = Tactic {act :: ((a -> to) -> from)}

apply :: (a -> b) -> Tactic b a ()
apply f = Tactic (\x -> f (x ()))
-- e.g. left :: Tactic (a \/ b) a () = apply Left

intro :: Tactic (a -> b) b a
intro = Tactic id

exact :: a -> Tactic a () ()
exact = Tactic . const

split :: Tactic a () () -> Tactic b () () -> Tactic (a, b) () ()
split (Tactic f) (Tactic g) = Tactic (\x -> (f x, g x))

assert :: Tactic a () () -> Tactic m m a
assert (Tactic f) = Tactic (\x -> x (f (\() -> ())))

-- theorem actions
data Theorem a = Proof (Tactic a () ())
runProof :: Theorem a -> a
runProof (Proof (Tactic f)) = f (\() -> ())

qed :: Tactic () () ()
qed = Tactic (const ())

data Axiom a
admit :: forall a. Axiom a = admit

-- main
main = return qed

--orComm :: Theorem (Either a b -> Either b a)
--orComm = do qed

