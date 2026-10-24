module Utils where

import Combinators
import Control.Monad.State -- mtl

x <&> f = fmap f x

-- untilM :: Monad m => (a -> Bool) -> m a -> m a from: utility-ht

loopUntil :: Monad m => (a -> Bool) -> StateT s m a -> s -> m [a]
loopUntil p m i = go i where
  go s = do
    (x, s') <- runStateT m s
    if p x then return [] else (x :) <$> go s'

-- also :: (a -> Bool) -> (a -> State s a) -> a -> State s [a]
untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m [a]
untilM p m = loopUntil p (get >>= lift . m)

untilM' :: Monad m => (a -> Bool) -> m a -> m [a]
untilM' p m = loopUntil p (lift m) ()
