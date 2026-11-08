module Utils where

import Combinators
import Control.Monad.State -- mtl
import Data.Bifunctor

(<&>) :: Functor f => f a -> (a -> b) -> f b
x <&> f = fmap f x

-- untilM :: Monad m => (a -> Bool) -> m a -> m a from: utility-ht

loopUntil :: (Monad m) => (a -> Bool) -> StateT s m a -> s -> m ([a], a)
loopUntil p m s = do
  (x, s') <- runStateT m s
  if p x then return ([], x) else first (x :) <$> loopUntil p m s'

-- also :: (a -> Bool) -> (a -> State s a) -> a -> State s ([a], a)
untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m ([a], a)
untilM p m = loopUntil p (get >>= lift . m)

untilM' :: (Monad m) => (a -> Bool) -> m a -> m ([a], a)
untilM' p m = loopUntil p (lift m) ()
