{-# LANGUAGE InstanceSigs #-}
-- | 

module Haskell.FirstPrinciples.ComposingTypes.Compose where

newtype Compose f g a =
  Compose { getCompose :: f (g a)}
  deriving (Eq, Show)

a = Compose [Just (1 :: Int), Nothing] 

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure (pure a)
  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f0) <*> (Compose a) =
    Compose $ fmap (<*>) f0 <*> a -- see my superwave op
