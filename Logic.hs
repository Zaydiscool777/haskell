{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures #-}
import Data.Void (Void, absurd)
import Data.Kind (Type)

{-import GHC.Base (TYPE, LiftedRep)
type Type = TYPE LiftedRep
data Void deriving (Eq, Show)
absurd :: Void -> a
absurd x = case x of {}
data Either a b = Left a | Right b deriving (Eq, Show)
either :: (a -> c) -> (b -> c) -> (Either a b) -> c
either a b c = case c of
  Left d -> a d
  Right d -> b d -}

main :: IO ()
main = putStrLn "Q.E.D.; quod erat demonstrandum"
-- this is curry-howard isomorphism of https://en.wikipedia.org/wiki/Intuitionistic_logic

type a :> b = a -> b -- implies
type a :* b = (a, b) -- and
type a :+ b = Either a b -- or
-- these two may be abnormally defined
type a :*> b = (b a, a) -- for all
type a :+> b = Either (b a) a -- there exists
data Unit = Unit deriving (Eq, Show)

type Not a = a -> Void
newtype To a b = Unto {unto :: b -> a}
class Unchair a where em :: (a :+ Not a) -- proves a is not false
instance Unchair Unit where em :: Unit :+ Not Unit; em = Left Unit
instance Unchair Void where em :: Void :+ Not Void; em = Right id

-- Not is 11 by default
infixl 7 :*
infixl 6 :+
infixr 4 :>
infix 3 :*>
infix 3 :+>
type a :-> b = a -> b
infix 2 :->

-- axioms
{- almost all axioms have an _In_troduction and a _El_imination version.
exceptions: coEl and diIn are split by -L and -R, vaIn is imIn Unit
the only other axiom not of In or El is imDs (distribution of im over im)
usually first two letters state the main propositon of axioms
implication, comjuntion, disjunction, value, universal & existential quantification -}
imIn :: a :-> b :> a; imEl :: (a, a :> b) :-> b; imDs :: a :> b :> c :-> ((a :> b) :> (a :> c))
imIn = const; imEl = uncurry (flip id); imDs = (<*>)
coIn :: (a, b) :-> a :* b; coElL :: a :* b :-> a; coElR :: a :* b :-> b
coIn = id; coElL = fst; coElR = snd
diInL :: a :-> a :+ b; diInR :: b :-> a :+ b; diEl :: (a :> c, b :> c, a :+ b) :-> c
diInL = Left; diInR = Right; diEl = untrurry either where untrurry f (a, b, c) = f a b c
vaEl :: Void :-> a; vaEl = absurd
-- unIn and exIn may be difficult to use normally
unIn :: a :> f a :-> a :> (a :*> f); unEl :: (x :*> To p) :-> x :> p
unIn = (>>=(,)); unEl = unto . fst
exIn :: f a :> a :-> (a :+> f) :> a; exEl :: x :> p :-> (x :+> To p)
exIn = flip either id; exEl = Left . Unto

-- set qed to final comp., and set proof to qed
imId :: a :-> a
imId a = qed
  where
    qed = a

-- specify type by inferred application of typing
-- assumptions are _similar_ to passing values
vaIn :: forall a. a :-> Unit
vaIn = qed
  where
    b :: Unit :-> (a :> Unit)
    b = imIn
    c = b Unit
    qed = c

-- first prove with imTr (a, b) p = qed, then move p to qed
imTr :: (a :> b, b :> c) :-> a :> c
imTr (a, b) = qed
  where
    c x = imEl (x, a)
    d x = imEl (c x, b)
    qed = d -- qed p = d p

imDt :: (a :> b :> c, c :> d) :-> a :> b :> d
imDt (a, b) = qed
  where
    e x = imTr (x, b)
    f = imTr (a, e)
    qed = f

-- ScopedTypeVariables allows for more utilizable typing
curry' :: forall a b c. a :* b :> c :-> a :> b :> c
curry' = qed
  where
    e :: a :-> (b :> a)
    e = imIn
    f :: b :-> b
    f = imId :: b :-> b
    g :: a :-> (b :> b)
    g = imIn f
    h :: a -> b -> a :* b
    h a b = coIn (e a b, g a b) -- crazy assumptions!
    i a b c = imEl (h b c, a)
    qed = i

contr' :: (Unchair a) => (a :* Not a) :-> Not a
contr' a = qed
  where
    b = coElR a
    qed = b

contr :: (Unchair a) => a :> Not a :> Not a
contr = curry' contr'

deimp :: forall a b. (Unchair a) => a :> b :-> b :+ Not a
deimp a = qed
  where
    -- case: a
    c :: a :-> (b :+ Not a)
    b x = imEl (x, a)
    c x = diInL (b x)
    -- case: not a
    d :: Not a :-> (b :+ Not a)
    d = diInR
    e :: b :+ Not a
    e = diEl (c, d, em)
    qed = e


peirce :: forall a b. (Unchair a) => ((a :> b) :> a) :-> a
peirce a = qed
  where
    qed = undefined -- todo