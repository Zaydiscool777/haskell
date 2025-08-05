{- A monad is defined by three things:
    - a type constructor, m (the monad, in a way)
    - a function, return
    - an operator, (>>=) (pronounced "bind")
return :: a -> m a
(>>=)  :: m a -> (a -> m b) -> m b -}
{-
let's assume that there is a database that has:
father :: Person -> Maybe Person
mother :: Person -> Maybe Person
Now, we could do:
maternalGrandfather :: Person -> Maybe Person
maternalGrandfather p =
    case mother p of
        Nothing -> Nothing
        Just mom -> father mom
But why not just: maternalGrandfather p = mother p >>= father?
We can also simplify what would otherwise be very wide and long functions:
bothGrandfathers p =
   father p >>=
       (\dad -> father dad >>=
           (\gf1 -> mother p >>=   -- gf1 is only used in the final return
               (\mom -> father mom >>=
                   (\gf2 -> return (gf1,gf2) ))))
When using the type Monad, you can use >> ("then"), which is
already implemented as: m >> n = m >>= \_ -> n
-}
printSomethingTwice str = putStrLn str >> putStrLn str
-- since Monad is a subclass of Applicative, you can also use fmap, pure, and <*>
-- also, *> & >> are equivalent when it comes to Monads, as are pure & return.
-- the & operator is defined as v & f = f v