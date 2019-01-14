-- |
module Haskell.Applicative.ListApplicativeExercise where

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

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = flip repeat' 5 <$> arbitrary  

repeat' :: a ->Int-> List a
repeat'  a 0 = Cons a Nil
repeat' a n = as
  where as = Cons a (repeat' a (n-1))

functions = Cons (+ 1) (Cons (* 2) Nil)

values = Cons 1 (Cons 2 Nil)

fv = functions <*> values

passive = ($ [1,2,3]) (flip fmap)
active = flip fmap $ [1,2,3]

main :: IO ()
main = do
  print fv
  print $ passive (+1)
  print $ active (+1)
  let trigger = undefined :: List (Int,String,Int)  
  quickBatch (applicative trigger)
