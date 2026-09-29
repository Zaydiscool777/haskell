{- The three laws of a monad are:
m >>= return = m (right unit)
return x >>= f = f x (left unit)
(m >>= f) >>= g  =  m >>= (\x -> f x >>= g) (associativity)
this also means:
(m >> n) >> o  =  m >> (n >> o), and
(f >=> g) >=> h  =  f >=> (g >=> h)
where >=> is the monad composition operator, definded as:
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g
(it's like ., but the arguments are flipped. <=< is flipped >=>.)
-}
{- in category theory, monads have a "alternative yet equivalent" definition.
monads are functors (fmap) with return and join :: M (M a) -> M a.
(>>=) can be defined as m >>= g = join (fmap g m). similarly:
fmap f x = x >>= (return . f)
join x = x >>= id
liftM is the Monad version of fmap, and ap is the Monad version of <*>.
you can define monads by:
instance Monad Foo where
    return = -- etc.
    (>>=) = -- etc.
instance Applicative Foo where
    pure = return
    (<*>) = ap
instance Functor Foo where
    fmap = liftM
-}