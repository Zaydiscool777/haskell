import Data.Monoid

newtype Disj a = Disj {getDisj :: a}
instance Semigroup (Disj Bool) where
  (<>) :: Disj Bool -> Disj Bool -> Disj Bool
  Disj x <> Disj y = Disj (x || y)
instance Monoid (Disj Bool) where
  mempty :: Disj Bool
  mempty = Disj False

newtype Conj a = Conj {getConj :: a}
instance Semigroup (Conj Bool) where
  (<>) :: Conj Bool -> Conj Bool -> Conj Bool
  Conj x <> Conj y = Conj (x || y)
instance Monoid (Conj Bool) where
  mempty :: Conj Bool
  mempty = Conj True
