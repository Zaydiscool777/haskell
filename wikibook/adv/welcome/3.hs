liftA5 :: Applicative f => (a -> b -> c -> d -> e -> k)
  -> f a -> f b -> f c -> f d -> f e -> f k
liftA5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e
