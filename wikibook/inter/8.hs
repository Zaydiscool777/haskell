import Data.Bifunctor
data Weird a b =
    First a
    | Second b
    | Third [(a, b)]
    | Fourth (Weird a b)
    | Fifth [Weird a b] a (Weird a a, Maybe (Weird a b))
weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
weirdMap fa fb = g where
    g (First a) = First (fa a)
    g (Second b) = Second (fb b)
    g (Third c) = Third (map (bimap fa fb) c) -- hlint says: bimap
    g (Fourth x) = Fourth (g x)
    g (Fifth d e f) = Fifth dr (fa e) fr where
        dr = map g d
        -- er = fa e
        fr = bimap (weirdMap fa fa) h f
        h (Just x) = Just (g x)
        h Nothing = Nothing

weirdFold ::
    (a -> r)
    -> (b -> r)
    -> ([(a,b)] -> r)
    -> (r -> r)
    -> ([r] -> a -> (Weird a a, Maybe r) -> r)
    -> Weird a b -> r
weirdFold f1 f2 f3 f4 f5 = g
  where
    g (First x) = f1 x
    g (Second y) = f2 y
    g (Third z) = f3 z
    g (Fourth w) = f4 (g w)
    g (Fifth a b c) = f5 (weirdMap g a) b (bimap g h c) -- weird a b =/= weird a a
        where
            h (Just x) = g x
            h Nothing = Nothing

{- weirdFold ::
    (r -> a -> r)
    -> (r -> b -> r)
    -> (r -> a -> b -> r)
    -> (r -> Weird a b -> r)
    -> r -> [Weird a b] -> r
weirdFold f1 f2 f3 f4 s x = foldl (weirdFold' f1 f2 f3 f4) s x
    where
        weirdFold' ::
            (r -> a -> r)
            -> (r -> b -> r)
            -> (r -> a -> b -> r)
            -> (r -> [Weird a b] -> r)
            -> r -> [Weird a b] -> r
        weirdFold' f1 f2 f3 f4 s = g
        g (First a) = f1 s a
        g (Second b) = f2 s b
        g (Third [z]) = f3 s (fst z) (snd z) -- hlint says: uncurry
        g (Fourth (x:xs)) = g (f4 s x)-}
