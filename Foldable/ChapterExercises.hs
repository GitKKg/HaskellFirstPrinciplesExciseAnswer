-- | 

module Haskell.FirstPrinciples.Foldable.ChapterExercises where

import Data.Monoid
import Data.Foldable

data Constant  a b =
  Constant a

-- instance Monoid a =>  Monoid ( Constant a b) where
  -- mappend (Constant a1) (Constant a2) = Constant (mappend a1 a2)
  -- mempty = Constant mempty

-- foldMap require monoid ,so to implemt Foldable with foldr is more verstile and shoud be do by default
instance  Foldable (Constant a) where
  foldr f b  (Constant a) = b

data Two a b =
  Two a b

instance Foldable (Two a) where
  foldr f b0 (Two a b) =  f b b0

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f b0 (Three a b c) = f c b0

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  -- foldr f bt (Three' a b1 b2) = f b2 bt -- b1 is ok too ...
  -- after one day rest,at the moment of waking up,get below more "foldable" answer
  -- foldr f bt (Three' a b1 b2) = f b2 (f b1 bt)
  foldr f b0 (Three' a b1 b2) = foldr f b0 [b1,b2]

data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  -- foldr f bt (Four' a b1 b2 b3) = f b3 bt
  -- foldr f bt (Four' a b1 b2 b3) = f b3 . f b2 $ f b1 bt
  foldr f bt (Four' a b1 b2 b3) = foldr f bt [b1,b2,b3]

filterF p = foldMap (\x -> if p x then pure x else mempty)
