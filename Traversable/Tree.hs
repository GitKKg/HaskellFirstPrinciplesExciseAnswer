-- | 

module Haskell.FirstPrinciples.Traversable.Tree where

import Data.Traversable
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a =
  Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node ta1 a ta2) = Node (f <$> ta1) (f a) (f <$> ta2)

instance Foldable Tree where
  foldMap fm Empty = mempty
  foldMap fm (Leaf a) = fm a
  foldMap fm (Node ta1 a ta2) =
    foldMap fm ta1 `mappend` foldMap fm (Leaf a) `mappend` foldMap fm ta2

instance Traversable Tree where
  traverse fb Empty = pure Empty
  traverse fb (Leaf a) = Leaf <$> fb a
  traverse fb (Node ta1 a ta2) =
    Node <$> traverse fb ta1 <*> fb a <*> traverse fb ta2

-- instance (Eq a) => EqProp (Tree a) where
  -- (=-=) = eq
  
