{-# LANGUAGE ScopedTypeVariables #-}
module Me.TaoQED where
-- this is a intuistionistic version of curry-howard isomorphism for https://teorth.github.io/QED/.
-- moral of the story: don't use haskell, or at least use a wrapper like the gdp package
import Data.Void
import Data.Either
import Data.Kind
-- Data.Type? Data.Constraint? Data.Typeable? GHC.Generics? GHC.TypeLits?

type Function :: Type -> Type -> Type -- implies, implication
type Function a b = a -> b
type a :-> b = Function a b
type Product :: Type -> Type -> Type -- and, conjunction
type Product a b = (a, b)
type a :* b = Product a b
type Sum :: Type -> Type -> Type -- or, disjunction
type Sum a b = Either a b
type a :+ b = Sum a b
type Unit :: Type -- true, truth
data Unit = It -- real Unit is from ghc-prim... aw man
-- type Void :: Type -- false, falsehood
-- type Void = Data.Void.Void
type DepProduct :: Type -> (Type -> Type) -> Type -- for all, universal quantifier
type DepProduct a b = (a, b a)
type a :*> b = DepProduct a b
type DepSum :: Type -> (Type -> Type) -> Type -- there exists, existensial quantifier
type DepSum a b = Either a (b a)
type a :+> b = DepSum a b

--e1
coIn :: (a, b) -> (a :* b)
coIn = id -- only works since Product IS 2 items bound together

e1'1 :: forall a b c. (a, b, c) -> (a :* b) :* c
e1'1 (a, b, c) = qed
  where
    d = coIn (a, b) :: a :* b
    qed = coIn (d, c) :: (a :* b) :* c


--e2
coElL :: (a :* b) -> a
coElL = fst
coElR :: (a :* b) -> b
coElR = snd

--e3
diInL :: a -> (a :+ b)
diInL = Left
diInR :: b -> (a :+ b)
diInR = Right

--e4~6
imIn :: a -> (b :-> a) -- here, :-> is assumption
imIn = const
imAs :: (a :-> b, b -> c) -> (a :-> c) -- here, :-> is assumption
imAs (a, b) = b . a -- first law confusing without points
imId :: a :-> a
imId = id

--e7
imPs :: a :-> (b :-> a)
imPs = const

--e8
imMp :: (a, a :-> b) -> b
imMp (a, i) = i a

--e9
diCa :: (a :-> b, c :-> b) -> ((a :+ c) :-> b)
diCa (a, b) = either a b

--e10
type Equal :: Type -> Type -> Type
type Equal a b = (a :-> b) :* (b :-> a)
type a := b = Equal a b
biIn :: (a :-> b, b :-> a) -> (a := b)
biIn = id -- see coIn
biElL :: (a := b) -> (a :-> b)
biElL = coElL
biElR :: (a := b) -> (b :-> a)
biElR = coElR

--e11
type Not :: Type -> Type
type Not a = a :-> Void -- unsure how else to define it
diElL :: (a :+ b, Not a) -> b
diElL (a, n) = either (absurd . n) id a
diElR :: (a :+ b, Not b) -> a
diElR (a, n) = either id (absurd . n) a

--e12
-- sad truth: this cannot be a value in intuitionistic logic. how weak.
diEm' :: a -> (a :+ Not a)
diEm' = Left
diEm'' :: Not a -> (a :+ Not a)
diEm'' = Right
-- this means many proofs from this point may take extra parameters and have 2 cases.
-- thankfully, however, we can replace it with this:
coAd :: (a :* Not a) -> b
coAd (a, n) = absurd (n a)

lcFa :: Not Void
lcFa = imId

main :: IO ()
main = putStrLn "Q.E.D."
