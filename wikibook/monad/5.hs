-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- f >=> g = \x -> f x >>= g;
import Control.Monad

safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog x
    | x > 0     = Just (log x)
    | otherwise = Nothing
safeSqrt :: (Floating a, Ord a) => a -> Maybe a
safeSqrt x
    | x > 0     = Just (sqrt x)
    | otherwise = Nothing

safeLogSqrt = safeLog <=< safeSqrt
unsafeLogSqrt = log . sqrt

phonebook :: [(String, String)]
phonebook = [ ("Bob",   "01788 665242"),
              ("Fred",  "01624 556442"),
              ("Alice", "01889 985333"),
              ("Jane",  "01732 187565") ]

x = lookup "Jeff" phonebook