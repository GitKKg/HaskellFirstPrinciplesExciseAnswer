-- | 

module Haskell.FirstPrinciples.MonadTransformers.StateT where

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s)}


--State should be serialized,order depedent
instance (Functor m) => Functor (StateT s m) where
  fmap f sma = StateT $ \s -> do
    let mas = runStateT sma s
    let astobs = \as -> ((f.fst) as, snd as)
    fmap astobs mas
    
instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a,s)
  StateT smab <*> StateT sma = StateT $ \s -> do
    as <- sma s
    abs <- smab (snd as)
    let a = fst as
    let ab = fst abs
    return (ab a, snd abs)

instance (Monad m) => Monad (StateT s m) where
  return = pure
  sma >>= f = StateT $ \s -> do
    (a,ns) <- runStateT sma s
    runStateT (f a) ns


