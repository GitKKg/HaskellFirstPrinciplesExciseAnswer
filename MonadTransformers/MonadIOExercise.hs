-- | 

module Haskell.FirstPrinciples.MonadTransformers.MonadIOExercise where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad
import Control.Applicative
-- import Control.Monad.Trans.Reader

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a}

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s)}

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (Just x))
  (MaybeT fab) <*> (MaybeT fa) = MaybeT $ (<*>) <$> fab <*> fa

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f = MaybeT $ do
    a <- ma
    case a of
      Nothing -> return Nothing
      Just a0 -> runMaybeT $ f a0

--la = liftA

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just
  
instance (MonadIO m) => MonadIO (MaybeT m) where
  -- pass ,and more explcit
  --liftIO ioa = MaybeT $ do liftM Just (liftIO ioa)
  liftIO = MaybeT . (liftM Just) . liftIO

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap f) . rma 

instance (Applicative m) => Applicative (ReaderT r m) where
  pure = ReaderT . const . pure
  ReaderT rmfab <*> ReaderT rma =
    -- also pass, note . means different with construct inside
    --ReaderT . flip (<*>) rma $ fmap (<*>) rmfab
    ReaderT $ fmap (<*>) rmfab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  ReaderT rma >>= famb = ReaderT $ \r -> do
    a <- rma r
    runReaderT (famb a) r
  
instance (MonadIO m) => MonadIO (ReaderT r m) where
  -- pass ,too
  -- liftIO ioa = ReaderT $ \r -> do
    -- liftIO ioa

  --actually implemented MonadTrans of ReaderT r
  liftIO = ReaderT . const . liftIO

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

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO ioa = StateT $ \s -> do
    a <- liftIO ioa
    return (a,s)
  
