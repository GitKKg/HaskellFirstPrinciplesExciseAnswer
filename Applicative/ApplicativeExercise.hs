
-- | 

module Haskell.Applicative.ApplicativeExercise where

import Control.Applicative
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Gen
import Data.Monoid

import Control.Applicative (liftA3)


arrayPure = pure :: a -> [] a
arrayApplicative = (<*>) :: [] (a->b) -> [] a -> [] b

ioPure = pure :: a -> IO a
ioApplicative = (<*>) :: IO (a->b) -> IO a -> IO b

-- just could be verified in ghci
-- :t  pure :: Monoid a => b -> (,) a b
-- :t  (<*>) :: Monoid c => (,) c (a->b) -> (,) c  a -> (,) c b
-- :t pure :: a -> (->) b a
-- :t  (<*>) :: Monoid c => (->) c (a->b) -> (->) c  a -> (->) c b

data Pair a = Pair a a deriving (Show,Eq)

instance Functor Pair where
   fmap f (Pair a b)  = Pair (f a) (f b)

-- instance Monoid a =>  Monoid (Pair a)  where
  -- mappend (Pair a b) (Pair c d) = Pair (a `mappend` c) (b `mappend` d )
  -- mempty = Pair mempty mempty

instance  Applicative Pair   where
  pure a  = Pair a a
  -- (<*>) :: Monoid b => Pair (a->b) -> Pair a -> Pair b  
  (<*>) (Pair f g)  (Pair a b) =
    Pair (f a) (g b)
    -- Pair   ( (f a) <*> (g a) )  ( (f b) <*>  (g b))   

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)
  
instance Eq a => EqProp (Pair  a) where
  (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
  

instance Monoid a =>  Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two a1 f) (Two a2 b) = Two (a1 `mappend` a2) (f b) 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Eq a,Eq b) => EqProp (Two a b) where
  (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance  (Monoid a, Monoid b ) =>  Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three a1 b1 f) (Three a2 b2 c) = Three (a1 `mappend` a2) (b1 `mappend` b2) (f c)

instance (Arbitrary a, Arbitrary b,Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Eq a,Eq b,Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Three' a b = Three' a b b deriving (Eq,Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a1 f1 f2) (Three' a2 b1 b2) = Three' (a1 `mappend` a2) (f1 b1) (f2 b2)

instance (Eq a,Eq b) => EqProp (Three' a b) where
  (=-=) = eq
instance (Arbitrary a, Arbitrary b ) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return (Three' a b1 b2)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance Monoid a => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  (<*>) (Four' a1 a2 a3 f)  (Four' b1 b2 b3 d) =
    Four' (a1 `mappend` b1) (a2 `mappend` b2) (a3 `mappend` b3) (f d)

instance (Eq a,Eq b) => EqProp (Four' a b) where
  (=-=) = eq
instance (Arbitrary a, Arbitrary b ) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return (Four'  a1 a2 a3 b)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos = liftA3 ((,,))  


main :: IO ()
main = do
  let trigger = undefined :: Pair  (Int,String,[Int])
  quickBatch $ applicative trigger
  let trigger = undefined :: Two String (Int,String,Int)
  quickBatch $ applicative trigger
  let trigger3 = undefined :: Three String [Int] (Int,Int,String)
  quickBatch $ applicative trigger3
  let trigger4 = undefined :: Three' String (Int,String,Int)
  quickBatch $ applicative trigger4
  let trigger5 = undefined :: Four' String (Int,String,Int)
  quickBatch $ applicative trigger5
