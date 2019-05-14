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
  pure :: a -> Compose f g a -- note : need LANGUAGE InstanceSigs on
  pure a = Compose $ pure (pure a)
  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f0) <*> (Compose a) =
    Compose $ fmap (<*>) f0 <*> a -- see my superwave op

instance (Foldable f, Foldable g) =>
          Foldable (Compose f g) where
  foldMap atom fga = foldMap (foldMap atom) $ getCompose fga

instance (Traversable f, Traversable g) =>
          Traversable (Compose f g) where
  traverse af0b fga = fmap Compose $ traverse (traverse af0b) $ getCompose fga

class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a c) = Deux (f a) (g c)

data Const a b = Const a

instance Bifunctor Const where
  bimap f g (Const a) = Const (f a)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a ) where
  bimap f g (Drei a b c) = Drei  a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d =
  Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

-- data Either0 a b =
  -- Left0 a
  -- | Right0 b

instance Bifunctor Either where
  bimap f g (Left a) = Left (f a)
  bimap f g (Right b) = Right (g b)
