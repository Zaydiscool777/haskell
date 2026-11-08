-- {-# LANGUAGE #-}
-- https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
data Doc =
  Nil
  | Text String Doc
  | Line Int Doc
  | Union Doc Doc
-- layout :: Doc -> String
nil = Nil
text s = Text s nil
line = Line 0 Nil

instance Semigroup Doc where
  Nil <> x = x
  (Text s y) <> x = Text s (y <> x)
  (Line i y) <> x = Line i (y <> x)
  (Union z y) <> x = Union (z <> x) (y <> x)
instance Monoid Doc where mempty = nil

nest _ Nil = Nil
nest i (Text s x) = Text s (nest i x)
nest i (Line j x) = Line (i + j) (nest i x)
nest i (Union x y) = Union (nest i x) (nest i y)

layout Nil = ""
layout (Text s x) = s ++ layout x
layout (Line i x) = '\n' : replicate i ' ' ++ layout x
layout (Union x y) = undefined

group Nil = Nil
group a@(Line _ x) = Union (Text " " (group x)) a
  -- Line does not pass group through?
group (Text s x) = Text s (group x)
group (Union x y) = Union (group x) y

flatten Nil = Nil
flatten (Text s x) = Text s (flatten x)
flatten (Line i x) = Text " " (flatten x)
flatten (Union x y) = flatten x

best _ _ Nil = Nil
best w k (Text s x) = Text s (best w (k + length s) x)
best w k (Line i x) = Line i (best w i x)
best w k (Union x y) = better w k (best w k x) (best w k y)

better w k x y = if fits (w - k) x then x else y

fits w _ | w < 0 = False
fits _ Nil = True
fits w (Text s x) = fits (w - length s) w
fits _ (Line _ _) = True

pretty w x = layout (best w 0 x)

main = pure ()
