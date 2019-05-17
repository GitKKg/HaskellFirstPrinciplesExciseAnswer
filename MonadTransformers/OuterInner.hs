-- | 

module Haskell.FirstPrinciples.MonadTransformers.OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return  1

aa :: MaybeT [] Int
aa = return 1

newtype Reader r a =
  Reader {runReader :: r ->a}
embeddedex :: MaybeT (ExceptT String (ReaderT () IO)) Int
embeddedex =
  -- pass
  --MaybeT $  ExceptT $  ReaderT $ const $ return (Right (Just 1))
  MaybeT $ ExceptT $ ReaderT $  fmap return (const (Right (Just 1)))
