-- # hiding Prelude(???)
-- flip f a b = f b a
-- infixr 9 .
-- (a . b) c = a (b c)
-- infixr 0 $
-- a $ b = a b
-- id a = a
-- const a b = b
-- (x <*> y) z = x z (y z)
import Data.Function hiding ((&))
-- # hiding Data.Function
-- fix :: (t -> t) -> t
-- fix f = let x = f x in x
-- applyWhen :: Bool -> (a -> a) -> a -> a
-- applyWhen = flip (bool id)
-- on :: (i -> i -> x) -> (a -> i) -> a -> a -> x
-- (op `on` f) x y = f x `op` f y -- on c f a b <> (<//>) a b f c

-- # quite common combinators

infixl 8 .:
(.:) :: (a -> b) -> (i -> j -> a) -> i -> j -> b
(.:) = (.) . (.)

($:) :: (a -> j -> x) -> a -> (i -> j) -> i -> x
($:) = (.) (.)

infixr 1 & -- a & b & f = f a b = (f $ a) $ b
(&) :: f -> (f -> v) -> v
(&) = flip id -- to be more precise, flip ($)

bool :: p -> p -> Bool -> p
bool t f c = if c then t else f

-- # SKI/BCKW combinator calculus

s :: (z -> y -> x) -> (z -> y) -> z -> x
s x y z = x z (y z)
-- s = ap -- s = (<*>)
{-Note: instance Applicative ((->) r) defines its <*> as f x (g x)...-}

k :: a -> b -> a
k = const

i :: a -> a
i = id

b :: (b -> c) -> (a -> b) -> a -> c
b = (.)

c :: (a -> b -> c) -> b -> a -> c
c = flip

w :: (x -> x -> r) -> x -> r
w = flip s id

-- apply with flipped argument order: it's a little weird, but somewhat common.
-- b' f g x = f (g x)
b' :: (((a -> v) -> v) -> c) -> a -> c
b' = (. (&))
-- note: since a $ b $ c = (a . b) c, you could develop an
-- uneven barrage of b' and . when there could've been
-- a very nice pattern.

-- # aliases

-- ## F#

infixl 0 |>
(|>) :: a -> (a -> b) -> b
(|>) = (&)
infixr 0 <|
(<|) :: (a -> b) -> a -> b
(<|) = ($)
infixr 0 >|
(>|) :: a -> (a -> b) -> b
(>|) = (&)
infixl 0 |<
(|<) :: (a -> b) -> a -> b
(|<) = ($)

-- ## Squiggol

(/->) :: (Monoid a) => [a] -> a
(/->) = mconcat
(//->) :: (Monoid a) => [a] -> [a]
(//->) = scanl (<>) mempty
(^|) :: (Ord a) => a -> a -> a
(^|) = max
(^/) :: (Ord a) => a -> a -> a
(^/) = min

-- ## shorten

fl :: (a -> b -> c) -> b -> a -> c
fl = b' . (.>)

-- # woke combinators (meta-, sorry if it's vulgar)

{-
      foo  ::      A      -> B -> C
    albert :: X -> A
                beth :: Y -> B
                         carol :: C -> Z
      bar  :: X             -> Y    -> Z
bar = foo $:: albert ~> beth ~>        carol
-}

infixl 1 $::
($::) :: (xins -> xret) -> ((xins -> xret) -> (ins -> ret)) -> ins -> ret
($::) = (&)

infixr 2 ~>
(~>) :: (ins -> pin) -> (pout -> out) -> ((pin -> pout) -> (ins -> out))
(~>) = ((. (.)) . (.)) . (.>)

-- # swapping applicators

ror2, rol2, swp2 :: is -> (is -> r) -> r
ror2 = (&)
rol2 = (&)

swp2 = (&)

-- ## 3-fold

ror3 :: a1 -> a2 -> (a1 -> a2 -> v) -> v
ror3 = b' (b' . (.>))
rol3 :: a1 -> (a2 -> a1 -> v) -> a2 -> v
rol3 = b' (.)
swp3 :: a1 -> a2 -> (a2 -> a1 -> v) -> v
swp3 = b' (b' . (.))

-- ### simpleton 3-fold: ___ f a b -> _ _ _

fab :: (t1 -> t2 -> t3) -> t1 -> t2 -> t3
fab = id
fba :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
fba = flip
afb :: t1 -> (t1 -> t2 -> t3) -> t2 -> t3
afb = (&) -- fba fab
abf :: t1 -> (t2 -> t1 -> t3) -> t2 -> t3
abf = b' (.)
bfa :: t1 -> t2 -> (t1 -> t2 -> t3) -> t3
bfa = b' (b' . (.>))
baf :: t1 -> t2 -> (t2 -> t1 -> t3) -> t3
baf = b' (b' . (.))

-- ## 4-fold

ror4 :: t1 -> t2 -> t3 -> (t1 -> t2 -> t3 -> t4) -> t4
ror4 = b' (b' . (b' .: ((.>) .: (.>))))

rol4 :: t1 -> (t2 -> t3 -> t1 -> t4) -> t2 -> t3 -> t4
rol4 = b' (.:)

-- ## 5-fold

ror5 :: t1 -> t2 -> t3 -> t4 -> (t1 -> t2 -> t3 -> t4 -> t5) -> t5
ror5 = b' (b' . (b' .: (((b' . (.>)) .:) . ((.>) .: (.>)))))
-- a % (b c) <> (%) a (b c) <> ((a %) . b) c

rol5 :: t1 -> (t2 -> t3 -> t4 -> t1 -> t5) -> t2 -> t3 -> t4 -> t5
rol5 = b' ((.) . (.) . (.))

-- ## n-fold?

-- # group-like class operators

(<<$>>) :: (Functor f) => (b -> c) -> f (a -> b) -> f (a -> c)
(<<$>>) = fmap . fmap

infixl 9 <.>
(<.>) :: (Functor f) => (t -> a -> b) -> t -> f a -> f b
(<.>) = (fmap .)

infixl 4 <*>>
(<*>>) :: (Applicative f) => f a -> f (a -> b) -> f b
(<*>>) = flip (<*>)

(<&&>) :: Applicative f => f a -> f b -> f (a,b)
(<&&>) = liftA2 (,)

-- # flipped combinators

infixl 9 .> -- a . b . c = c .> b .> a
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

infixr 9 !!>
(!!>) :: Int -> [c] -> c
(!!>) = flip (!!)

infixr 6 ><
(><) :: (Semigroup a) => a -> a -> a
(><) = flip (<>)

-- # other combinators

-- (<//>) :: a -> a -> (a -> i) -> (i -> i -> x) -> x
(<//>) :: (Functor f, Functor g) =>
  f a -> g a -> (a -> i) -> (f i -> g i -> x) -> x
(a <//> b) f c = (c <$> (<$> a) <*> (<$> b)) f
-- (a <//> b) f c = f a `c` f b
-- (a <//> b) f = b <//> a ((&) . f) (.)
-- a <//> b = (<$> a) /<>/ (<$> b)
-- a <//> b = a <//> b (flip fmap) (/<>/)

-- inverted <//>, in a way: 2f1v <-> 2v1f
(/<>/) :: (a -> i) -> (a -> j) -> a -> (i -> j -> x) -> x
-- (f /<>/ g) a c = f a `c` g a
(f /<>/ g) a c = (c <$> f <*> g) a

swing :: (((a -> b) -> b) -> c -> d) -> c -> a -> d
swing = flip . b'
-- eg. swing map :: [a -> b] -> a -> [b] for (a -> b) -> [a] -> [b]

tap :: Monad m => (b -> m a) -> b -> m b
tap = flip flip (>>) . (/<>/ return)

cross :: (t1 -> a) -> (t2 -> b) -> (t1, t2) -> (a, b)
cross f g (x, y) = (f x, g y)

dot :: Int -> (a -> a) -> a -> a
dot = (.: iterate) . (!!>)

-- mrconcat :: (Foldable t, Monoid m) => t m -> m
-- mrconcatF = foldr (<>) mempty

----------------------Tips-------------------------

main :: IO ()
main = putStrLn "this is a library not a program" -- dimwit

{-
<> means that they mean the same thing
% is an operator
flip $ . are in prelude
pointfree.io
en.wikipedia.org/wiki/Bird%E2%80%93Meertens_formalism

f a b = c <> f = \a -> \b -> c
\x -> a x <> a
f (g x) <> (f . g) x
a $ b $ c <> a (b c)
a $ b $ c $ d $ x <> (a . b . c . d) x
a % b <> (%) a b
(%) x a <> (% a) x
(a %) <> (%) a
f (x %) <> (f . (%)) x
a b c <> (a b) c
a b c d e f <> (a b c) d e f
f x a <> flip f a x
f (g a) b <> flip f b (g a)
(% a) <> flip (%) a
a % (b c) <> ((a %) . b) c

other things are in this library

($ a) <> (a &) <> (&) a

-- Bird-Meertens formalism

map f . concat <> concat . map (map f)
where concat :: [[a]] -> [a] = join
red . concat <> red . map red
where red (%) [e1,e2..en] = e%e1%e2..en, e is identity
red (+) . map (red (*)) . tails <> red (%)
where (%) = (succ .) . (*)
map f . map g <> map (f . g)

-}
