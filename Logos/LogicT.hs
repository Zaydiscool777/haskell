{-# LANGUAGE LambdaCase, BlockArguments #-}
module Logos.LogicT where
-- https://okmij.org/ftp/papers/LogicT.pdf
-- https://hackage.haskell.org/package/logict
import Data.List
import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Identity

-- Nothing <- msplit empty
-- Just (-> a, m) <- msplit (-> a <|> m)
class MonadPlus m => MonadLogic m where
  msplit :: m a -> m (Maybe (a, m a))
  interleave :: m a -> m a -> m a -- fair <|> (or)
  interleave x y = msplit x >>= \case
    Nothing -> y
    Just (xh, xt) -> return xh <|> interleave y xt
  (>>-) :: m a -> (a -> m b) -> m b -- fair >>= (and)
  x >>- f = msplit x >>= \case
    Nothing -> empty
    Just (xh, xt) -> f xh `interleave` (xt >>- f)
  ifte :: m a -> (a -> m b) -> m b -> m b -- soft cut
  ifte b y n = msplit b >>= \case
    Nothing -> n
    Just (bh, bt) -> y bh <|> (bt >>= y) -- n is not run on a backtrack
  once :: m a -> m a -- run once (for efficiency)
  once x = msplit x >>= \case
    Nothing -> empty
    Just (xh, _) -> return xh

gnot :: MonadLogic m => m a -> m ()
gnot m = ifte (once m) (const mzero) (return ())

instance MonadLogic [] where msplit = return . uncons

reflect :: MonadPlus m => Maybe (a, m a) -> m a
reflect Nothing = mzero
reflect (Just (ah, at)) = return ah `mplus` at
rr :: MonadLogic m => m a -> m a
rr = msplit >=> reflect

{-
newtype SSG a = Stream {unSSG :: [a]}
instance Functor SSG where fmap = liftM
instance Applicative SSG where pure e = Stream [e]; (<*>) = ap
instance Monad SSG where (Stream es) >>= f = Stream (es >>= unSSG . f)
-}

-- (value -> fail -> succeed) -> fail -> result
-- (head -> tail -> cons) -> null -> list
newtype LogicT m a = LogicT {unLogicT :: forall r. (a -> m r -> m r) -> m r -> m r}

instance Functor (LogicT m) where
  fmap f (LogicT l) = LogicT \rs -> l (rs . f)
instance Applicative (LogicT m) where
  pure x = LogicT ($ x)
  LogicT f <*> LogicT x = LogicT \rs -> f \gs -> x (rs . gs)
instance Monad (LogicT m) where
  LogicT x >>= f = LogicT \rs -> x (\v -> unLogicT (f v) rs)
instance Alternative (LogicT m) where
  empty = LogicT (const id)
  LogicT x <|> LogicT y = LogicT \rs -> x rs . y rs
instance MonadPlus (LogicT m)
instance MonadFail (LogicT m) where fail _ = empty
instance Semigroup (LogicT m a) where (<>) = (<|>)
instance Monoid (LogicT m a) where mempty = empty
instance MonadTrans LogicT where lift x = LogicT \rs rf -> x >>= \v -> rs v rf
instance MonadIO m => MonadIO (LogicT m) where liftIO = lift . liftIO

instance Monad m => MonadLogic (LogicT m) where
  msplit :: LogicT m a -> LogicT m (Maybe (a, LogicT m a))
  msplit (LogicT x) = lift (x rs (return Nothing))
    where rs v bf = return (Just (v, lift bf >>= reflect))

solve :: Applicative m => LogicT m a -> m [a]
solve (LogicT x) = x (fmap . (:)) (pure [])
observe :: Alternative m => LogicT m a -> m a
observe (LogicT x) = x (const . pure) empty -- empty ~ fail "no answer"

bag :: Monad m => Maybe Int -> LogicT m a -> LogicT m [a]
bag (Just n) _ | n <= 0 = empty
bag n x = msplit x >>= \case
  Nothing -> empty
  Just (xh, xt) -> (xh:) <$> bag (pred <$> n) xt

runL :: Maybe Int -> LogicT Identity a -> [a]
runL n m = maybe id take n $ runIdentity (solve m) -- no bag?
