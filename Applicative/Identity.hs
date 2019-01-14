-- | 

module Haskell.Applicative.Identity where

import Control.Applicative

newtype Identity a = Identity a
  deriving (Eq , Ord , Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a


instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f ) (Identity a) = Identity (f a) -- just same with fmap 


newtype Constant a b =
  Constant {getConstant :: a}
  deriving (Eq , Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a


instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  (<*>) (Constant a) (Constant b) = Constant (a `mappend` b)


