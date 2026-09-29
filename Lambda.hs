{-# LANGUAGE PatternSynonyms #-}

import Data.Bool
import Data.Char
import Data.List

data Expr a = Abstr (Expr a) | Appl (Expr a) (Expr a) | Vari Int
  | Ext a deriving (Eq) -- unbound

func' :: Expr a -> Int -> (Expr a -> Expr a)
func' (Abstr x) user@0 z = func' x 1 z
func' (Abstr a) n z = Abstr (func' a (succ n) z)
func' (Appl a@(Abstr x) b) n z = func' (func' a 0 b) n z -- yes
func' (Appl a b) n z = Appl (func' a n z) (func' b n z) -- maybe?
func' (Vari a) n z = if a == n then z else Vari a 
func' (Ext x) _ z = Ext x -- error "func' ext"

-------Extra functions-------------------------------

(<//>) :: a -> a -> (a -> b) -> (b -> b -> c) -> c
(x <//> y) f c = f x `c` f y
-- <//> easier to type than <*>$

pattern (:>) :: Expr a -> Expr a -> Expr a
pattern a :> b = Appl a b

----For Expr-----------

type Varn = Int

func :: Expr a -> Expr a -> Expr a
func x = func' x 0
eunc :: Expr a -> a -> Expr a
eunc x y = func' x 0 (Ext y)

data Undef = Undef
instance (Read Undef) where
  readsPrec :: Int -> ReadS Undef
  readsPrec = const (singleton . (Undef,))
instance (Show Undef) where
  show :: Undef -> String
  show = const ""
type PExpr = Expr Undef

instance (Show a) => Show (Expr a) where
  show :: Show a => Expr a -> String
  show (Abstr x) = "/ " ++ show x
  show (Appl x y) = "`(" ++ show x ++ ") (" ++ show y ++ ")"
  show (Vari x) = '.' : show x
  show (Ext (x :: a)) = '!' : show x

instance (Read a) => Read (Expr a) where
  readsPrec :: Read a => Int -> ReadS (Expr a)
  readsPrec _ s =
    case parse s of
      [] -> []
      xs -> [(foldl1 Appl xs, "")]
    where -- todo: turn () = () into case clauses with error being parse returns []
      parse :: (Read a) => String -> [Expr a]
      parse "" = []
      parse ('/':r) = Abstr a:b
        where (a:b) = parse r
      parse ('λ':r) = Abstr a:b
        where (a:b) = parse r
      parse ('\'':r) = Appl a b:c
        where (a:b:c) = parse r
      parse ('.':r) = Vari (read a :: Varn):parse b
        where (a, b) = span isDigit r
      parse ('!':r) = Ext a:parse b
        where ((a :: a, b):_) = reads r
      -- parse ('#':r) = parse $ tail $ dropWhile (/='#') r
      parse (_:r) = parse r


-------SKI calculus---------------------------------

data SKI a = S | K | I
  | ApplS (SKI a) (SKI a)
  | Inv Varn | InvA (SKI a)
  | ExtS a deriving (Eq, Show)

frSKI :: SKI a -> Expr a
frSKI I = Abstr (Vari 1)
frSKI K = Abstr (Abstr (Vari 2))
frSKI S = Abstr (Abstr (Abstr (Appl (Appl (Vari 3) (Vari 1)) (Appl (Vari 2) (Vari 1)))))
frSKI (ApplS x y) = (x <//> y) frSKI Appl
frSKI (Inv x) = Vari x -- shouldn't happen
frSKI (InvA x) = Abstr (frSKI x) -- shouldn't happen
frSKI (ExtS x) = Ext x

{-
we can use the bracket abstraction technique:
[x]x = I
[x]c | x {~ c = K c
[x](p q) = S ([x]p) ([x]q)
also:
[x](p x) = p
[x](\v.w) | x {= w = [x](\v.([x]w))
how will this work in de bruijn indices?
[x](\v.w) | x {= w = [x](\v.([x+1]w))
-}

toSKI :: Expr a -> SKI a
toSKI = box 1 . prSKI
  where
    prSKI :: Expr a -> SKI a
    prSKI (Abstr x) = InvA (prSKI x)
    prSKI (Vari x) = Inv x
    prSKI (Appl x y) = (x <//> y) prSKI ApplS
    prSKI (Ext x) = ExtS x
    box :: Varn -> SKI a -> SKI a
    box v (InvA (Inv a)) | a == v = I
    box v (InvA a) | a `hasFree` v = ApplS K (box v a)
      where
        hasFree :: SKI a -> Varn -> Bool
        hasFree (Inv a) v = a /= v
        hasFree (InvA a) v = a `hasFree` succ v
        hasFree (ApplS a b) v = (a <//> b) (`hasFree` v) (||)
        hasFree _ _ = True
    box v (ApplS a b) = ApplS (ApplS S (box v a)) (box v b)
    box v (InvA a) = box v $! InvA (box (succ v) a)
    box _ x = x

---------------------------------------------------

main :: IO ()
a :: PExpr
a = Abstr (Appl (Vari 2) (Appl (Vari 1) (Vari 1))) -- read "/`.2`.1.1"
main = print $ func a a -- `(.2) (`(/ `(.2) (`(.1) (.1))) (/ `(.2) (`(.1) (.1)))) -- `.2`!a!a
