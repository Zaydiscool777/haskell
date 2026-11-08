{-# LANGUAGE BlockArguments, ApplicativeDo, LambdaCase #-}
-- https://hackage.haskell.org/package/attoparsec/docs/src/Data.Attoparsec.Zepto.html
import Control.Applicative
import Control.Monad
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.State.Lazy
import Control.Monad.Identity
--import Control.Monad.Trans -- from State as well
import Data.Char
import Data.Functor
import Data.Maybe
import Data.List
import qualified Data.List.NonEmpty as N
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Bifunctor

data Res a t = Ok a t | No t deriving Show
newtype ParserT t m a = ParserT {runParserT :: t -> m (Res a t)}

-- max. Alternative c -> context-free languages

instance Functor c => Functor (ParserT t c) where
  fmap f (ParserT x) = ParserT $
    (. x) $ fmap \case
      Ok a s' -> Ok (f a) s'
      No s' -> No s'
instance Monad c => Applicative (ParserT t c) where
  pure x = ParserT $ pure . Ok x
  ParserT f <*> ParserT x = ParserT $
    f >=> \case
      Ok a s' -> do
        x' <- x s'
        return case x' of
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
empty' :: Applicative c => ParserT t c a
empty' = ParserT $ pure . No
ParserT x <|>> ParserT y = ParserT $
  x >=> \case
    Ok a s' -> pure (Ok a s')
    No s' -> y s' -- (<|>) but no backtrack

instance Monad c => Monad (ParserT t c) where
  ParserT x >>= f = ParserT $
    x >=> \case
      Ok a s' -> let ParserT g = f a in g s'
      No s' -> return (No s')
instance Monad c => MonadPlus (ParserT t c)
instance Monad c => MonadFail (ParserT t c) where fail _ = mzero

instance Monad c => Semigroup (ParserT t c a) where (<>) = (<|>)
instance Monad c => Monoid (ParserT t c a) where mempty = empty

instance MonadTrans (ParserT t) where
  lift = ParserT . (. flip Ok) . flip fmap
instance MonadIO c => MonadIO (ParserT t c) where liftIO = lift . liftIO
instance Monad c => MonadState t (ParserT t c) where
  state = ParserT . (return .) . (uncurry Ok .)

{- import Data.Functor.Invariant
newtype ApplyT c a t = ApplyT {bruh :: ParserT t c a}
instance Monad c => Invariant (ApplyT c a) where
  invmap f f' (ApplyT p') = ApplyT (aux p')
    where
    aux (ParserT p) = ParserT \i -> do
      res <- p (f' i)
      pure case res of
        Ok a s -> Ok a (f s)
        No s -> No (f s) -}

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

eqs :: (Monad c, Eq a) => [a] -> ParserT [a] c [a]
eqs = mapM eq

notP :: (Alternative c, Monad c) => c a -> c ()
notP = optional >=> guard . isNothing

eof :: Monad c => ParserT [a] c ()
eof = notP item

manch :: Applicative c => (a -> Bool) -> ParserT [a] c [a]
-- different from (many . sat) in that it succeeds once?
manch p = ParserT \s -> pure let (a, s') = scan s in Ok a s'
  where
    scan (x:xs) | p x = let (a, s) = scan xs in (x:a, s)
    scan s = ([], s)

somch :: Monad c => (a -> Bool) -> ParserT [a] c [a]  
somch p = ParserT \s -> pure
    case scan1 s of
      Left (a, s') -> Ok a s'
      Right s' -> No s'
  where
    scan1 (x:xs) | p x = Left (scan (x:xs))
    scan1 s = Right s
    scan (x:xs) | p x = let (a, s) = scan xs in (x:a, s)
    scan s = ([], s)

choice :: (Alternative c, Foldable l) => l (ParserT t c a) -> ParserT t c a
choice = foldr (+++) empty'

skSpaces :: Parser ()
skSpaces = void $ manch isSpace

brack :: Applicative c => c x -> c y -> c a -> c a
(open `brack` close) p = open *> p <* close
quote q = brack q q

vopt p = void p <|> return ()

chainl1 p op = p >>= rest
  where
    rest x = do
      f <- op
      y <- p
      rest (f x y) +++ return x

sepBy p sep = liftA2 (:) p (many (sep *> p))

semiBy p sep = many (p <* sep)

ops :: (Monad c, Alternative c) => [(ParserT t c a, b)] -> ParserT t c b
ops x = choice [p >> return op | (p, op) <- x]

tails' :: ParserT [a] [] [a]
tails' = ParserT \s -> zipWith Ok (inits s) (tails s)

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
