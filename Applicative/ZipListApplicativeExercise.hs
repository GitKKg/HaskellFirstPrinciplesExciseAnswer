-- | 

module Haskell.Applicative.ZipListApplicativeExercise where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a lb) = Cons (f a) (fmap f (lb))

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  -- _ <*> Nil = Nil
  --train of thought: first iter data ,then iter function
  -- Cons f lf <*> Cons da ldata = (f <$> (Cons da ldata)) `append` ( lf <*> Cons da ldata)
  Cons f lf <*> Cons da ldata =
    flatMap (($ (Cons da ldata)) (flip fmap)) (Cons f lf)

-- instance Arbitrary a => Arbitrary (List a) where
  -- arbitrary = Cons <$> arbitrary (Nil <$> arbitrary)

instance Eq a => EqProp (List a) where
  (=-=) = eq

take' :: Int -> List a -> List a
take' 0 (Cons a la) = Nil
take' n (Cons a la) = Cons a (take' (n-1) la)
take' _ Nil = Nil


repeat' :: a ->Int-> List a
repeat'  a 0 = Cons a Nil
repeat' a n = as
  where as = Cons a (repeat' a (n-1))


newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq , Show)

-- book example actually means like this, just control.applicative.ziplist
newtype ZipList1 a =
  ZipList1 ([] a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 30 l
          ys' = let (ZipList' l) = ys
                in take' 30 l

instance Functor ZipList' where
  fmap _ (ZipList' Nil) = ZipList' Nil
  fmap f (ZipList' xs) = ZipList' $ fmap f xs
  

instance Applicative ZipList' where
  pure a = ZipList' (repeat' a 50)
  ZipList' Nil <*> _ = ZipList' Nil
  -- ZipList' lf  <*> ZipList' ldata = ZipList' (lf <*> ldata)
  ZipList' lf <*> ZipList' ldata = ZipList' (ziplist lf ldata)
  
ziplist :: List (a->b) ->List a -> List b
ziplist (Cons f lf) (Cons a la) = Cons (f a) ( ziplist lf la)
ziplist Nil _ = Nil
ziplist _  Nil = Nil


instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$>  ( flip repeat' 2 <$> arbitrary)  

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = flip repeat' 5 <$> arbitrary  

main :: IO ()
main = do
  let z = ZipList [(+9),(*2),(+8)]
  let z' = ZipList [1..2]
  print $ z <*> z'
  let z = ZipList' (Cons (+9) (Cons (*2) (Cons (+8) Nil)))
  let z' = ZipList' (Cons 1 (Cons 2 (Cons 3 Nil)))
  let zl = ZipList' $ repeat' 12 300
  print $ z <*> z'
  print $ z <*> zl
  let trigger = undefined :: List (Int,String,Int)  
  quickBatch (applicative trigger)
  let trigger = undefined :: ZipList' (Int,String,Int)
  quickBatch (applicative trigger)
