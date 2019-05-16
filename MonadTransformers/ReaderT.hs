{-# LANGUAGE InstanceSigs #-}
-- | 

module Haskell.FirstPrinciples.MonadTransformers.ReaderT where

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a}

instance (Functor m)  => Functor (ReaderT r m) where
  -- pass too
  --fmap f (ReaderT rma) = ReaderT $ (fmap f) . rma
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT (pure (pure a))
  (ReaderT fmab) <*> (ReaderT rma) = -- (->) is also applicative
    ReaderT $ fmap (<*>) fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= armb =
    ReaderT $ \r -> do
    a <- rma r
    runReaderT  (armb a) r 
    
