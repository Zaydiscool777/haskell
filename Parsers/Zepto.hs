{-# LANGUAGE BlockArguments, ApplicativeDo, LambdaCase, ViewPatterns #-}
-- https://hackage.haskell.org/package/attoparsec/docs/src/Data.Attoparsec.Zepto.html
import Control.Applicative
import Control.Monad
import Control.Monad.Fail ()
import Control.Monad.State.Lazy
import Data.Functor

data Res a t = Ok a t | No t deriving Show
newtype ParserT t m a = ParserT {runParserT :: t -> m (Res a t)}

-- max. Alternative c -> context-free languages

instance Functor c => Functor (ParserT t c) where
  fmap f (ParserT x) = ParserT $ (. x) $ fmap \case
    Ok a s' -> Ok (f a) s'
    No s' -> No s'
instance Monad c => Applicative (ParserT t c) where
  pure x = ParserT $ pure . Ok x
  ParserT f <*> ParserT x = ParserT $ f >=> \case
    Ok a s' -> x s' <&> \case
      Ok b s'' -> Ok (a b) s''
      No s'' -> No s''
    No s' -> pure (No s')
instance Monad c => Alternative (ParserT t c) where
  empty = ParserT $ pure . No
  ParserT x <|> ParserT y = ParserT \s ->
    x s >>= \case
      Ok a s' -> pure (Ok a s')
      No _ -> y s
-- technically empty only needs Applicative c, but <|> needs Monad c so yeah
pure' :: Applicative m => a -> ParserT t m a
pure' x = ParserT $ pure . Ok x
empty' :: Applicative c => ParserT t c a
empty' = ParserT $ pure . No
(<|>>) :: Monad m => ParserT t m a -> ParserT t m a -> ParserT t m a
ParserT x <|>> ParserT y = ParserT $ x >=> \case
  Ok a s' -> pure (Ok a s')
  No s' -> y s' -- (<|>) but no backtrack

instance Monad c => Monad (ParserT t c) where
  ParserT x >>= f = ParserT $ x >=> \case
    Ok a s' -> let ParserT g = f a in g s'
    No s' -> return (No s')
instance Monad c => MonadPlus (ParserT t c)
instance Monad c => MonadFail (ParserT t c) where fail _ = empty

instance Monad c => Semigroup (ParserT t c a) where (<>) = (<|>)
instance Monad c => Monoid (ParserT t c a) where mempty = empty

instance MonadTrans (ParserT t) where
  lift = ParserT . (. flip Ok) . flip fmap
instance MonadIO c => MonadIO (ParserT t c) where liftIO = lift . liftIO
instance Monad c => MonadState t (ParserT t c) where
  state = ParserT . (return .) . (uncurry Ok .)

{- import Control.Monad.Trans.Maybe
type ParserMST t c a = MaybeT (StateT t c) a -}

puresr :: Applicative c => (t -> Res a t) -> ParserT t c a
puresr = ParserT . (pure .)

type Parser a = ParserT String [] a
----------------------------------------------------
item :: Applicative c => ParserT [a] c a -- get
item = puresr \case
  [] -> No []
  x:xs -> Ok x xs

(+++) :: Alternative c => ParserT t c a -> ParserT t c a -> ParserT t c a
ParserT p +++ ParserT q = ParserT \s -> p s <|> q s

(<++) :: ParserT t [] a -> ParserT t [] a -> ParserT t [] a
ParserT p <++ ParserT q = ParserT \s ->
  p s >>= \case
    No _ -> q s -- (>>=) but no backtrack
    x -> pure x

-- look = get
-- pfail = empty

sat :: (Monad c, Eq a) => (a -> Bool) -> ParserT [a] c a
sat p = do
  c <- item
  if p c then pure c else empty'

eq :: (Monad c, Eq a) => a -> ParserT [a] c a
eq = sat . (==)

-- eqs :: (Monad c, Eq a) => [a] -> ParserT [a] c [a]
-- eqs = mapM eq

notP :: (Alternative c, Monad c) => c a -> c ()
notP = optional >=> \case {Nothing -> pure (); _ -> empty}

-- eof :: Monad c => ParserT [a] c ()
-- eof = notP item

manch :: Applicative c => (a -> Bool) -> ParserT [a] c [a]
-- different from (many . sat) in that it succeeds once?
manch p = ParserT \(scan -> (a, s')) -> pure (Ok a s')
  where
    scan (x:(scan -> (a, s))) | p x = (x:a, s)
    scan [] = ([], [])

somch :: Monad c => (a -> Bool) -> ParserT [a] c [a]  
somch p = puresr \s ->
  case scan1 s of
    Left (a, s') -> Ok a s'
    Right s' -> No s'
  where
    scan1 (x:xs) | p x = Left (scan (x:xs))
    scan1 [] = Right []
    scan (x:(scan -> (a, s))) | p x = (x:a, s)
    scan [] = ([], [])

choice :: (Alternative c, Foldable l) => l (ParserT t c a) -> ParserT t c a
choice x = ParserT \s -> foldr ((<|>) . flip runParserT s) empty x -- a price (asum) to pay

-- skSpaces :: Parser ()
-- skSpaces = void $ manch isSpace

brack :: Applicative c => c x -> c y -> c a -> c a
(open `brack` close) p = open *> p <* close
-- quote q = brack q q

vopt :: Alternative c => c a -> c ()
vopt p = void p <|> pure ()

chainl1 :: (Monad c, Alternative c) => ParserT t c b -> ParserT t c (b -> b -> b) -> ParserT t c b
chainl1 p op = p >>= rest
  where
    rest x = do
      f <- op
      y <- p
      rest (f x y) +++ return x

sepBy :: Alternative c => c a -> c b -> c [a]
sepBy p sep = liftA2 (:) p (many (sep *> p))

semiBy :: Alternative c => c a -> c b -> c [a]
semiBy p sep = many (p <* sep)

ops :: Alternative c => [(ParserT t c a, b)] -> ParserT t c b
ops = choice . fmap (uncurry ($>)) -- [p $> op | (p, op) <- x]

-- tails' :: ParserT [a] [] [a]
-- tails' = ParserT \s -> zipWith Ok (inits s) (tails s)

cont :: Applicative c => ParserT a c a
cont = ParserT \s -> pure (Ok s s)

{-
main :: IO ()
main = void do ------------------------------------
  let (Ok x _:_) = runParserT ini "x.yy.z = 'hello'\nw = 'no'\nx.yy = 'what'\n"
  print x
  return ()

data Inis a = Dict [(String, Inis a)] | Item a deriving Show

ini = dini <$> (semiBy set (eq '\n') <* eof)
set = do
  x <- somch isAlphaNum `sepBy` eq '.'
  skSpaces
  eq '='
  skSpaces
  y <- quote (eq '\'') (manch (/= '\''))
  return (x, y)
dini inp =
  case find (null . fst) inp of
    Just (_, x) -> Item x
    Nothing -> Dict $ map (second dini . rmpre) $
      N.groupBy
      (\(a:|_, _) (b:|_, _) -> a == b)
      $ sortOn (N.head . fst) non
  where
    non = flip mapMaybe inp \(a, b) -> do
      ax <- N.nonEmpty a
      return (ax, b)
    rmpre a@((b:|_, _):|_) = (b, [(c, d) | (_b:|c, d) <- N.toList a])
-}

{-
data Exp = Abs Exp | App Exp Exp | Var Integer deriving Show

sub :: Integer -> Exp -> Exp -> Exp
sub n r (Var x) = if x == n then r else Var x
sub n r (Abs x) = Abs (sub (n + 1) r x)
sub n r (App x y) = appl (sub n r x) (sub n r y)
appl :: Exp -> Exp -> Exp
appl (Abs x) y = sub 0 y x
appl x y = App x y

blcp :: Monad c => ParserT [Bool] c Exp
blcp = do
  a <- item
  if a
    then do
      v <- manch id
      item
      pure $ Var (toInteger (length v))
    else do
      b <- item
      if b
        then App <$> blcp <*> blcp
        else Abs <$> blcp

charToBits :: Char -> [Bool]
charToBits c = [testBit (fromIntegral (ord c) :: Word8) i | i <- [7,6..0]]

blToL :: [Bool] -> Exp
blToL [] = Abs (Abs (Var 0))
blToL (x:r) = let w = Abs (Abs (Var if x then 1 else 0)) in Abs (appl (appl (Var 0) w) (blToL r))

lToBl :: Exp -> [Bool]
lToBl (Abs (Abs (Var 0))) = []
lToBl (Abs (App (App (Var 0) (Abs (Abs (Var x)))) r)) = ((x == 1):lToBl r)
lToBl _ = []

bitsToChar :: Monad c => ParserT [Bool] c Char
bitsToChar = -- (\x -> if x then '1' else '0') <$> item
  chr . foldl (\a b -> a * 2 + if b then 1 else 0) 0
    <$> replicateM 8 item

blc :: Exp -> String -> String
blc e s = go i
  where
    i = (lToBl . appl e . blToL) (s >>= charToBits)
    go b = case runIdentity (runParserT bitsToChar b) of
      No _ -> []
      Ok !c rest -> c:go rest

main :: IO ()
main = do
  e <- getLine
  e' <- do
    Ok e' _ <- runParserT blcp $ map (== '1') e
    return e'
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  interact (blc e')
-}
