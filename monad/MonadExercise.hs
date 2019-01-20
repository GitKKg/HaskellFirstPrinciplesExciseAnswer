-- | 

module MonadExercise where

import Control.Monad
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

data PhEither b a =
  Leftt a
  | Rightt b
  deriving (Eq,Show)


instance Functor  (PhEither b ) where
  fmap f (Leftt a) = Leftt (f a)
  fmap f (Rightt b) = Rightt b

instance Applicative (PhEither b) where
  pure a = Leftt a
  (<*>) (Leftt f) (Leftt a) = Leftt (f a)
  (<*>) (Leftt f) (Rightt b) = Rightt b
  (<*>) (Rightt f) (Rightt b) = Rightt f
  (<*>) (Rightt f) (Leftt a) = Rightt f

instance (Arbitrary b ,Arbitrary a) => Arbitrary (PhEither b a) where
  arbitrary = frequency [(1, Leftt <$> arbitrary)
                        ,(1, Rightt <$> arbitrary)
                        ]  
instance Monad (PhEither b) where
  return = pure
  (>>=) (Leftt a) f = f a
  (>>=) (Rightt b) f = Rightt b

instance (Eq a , Eq b) => EqProp (PhEither a b) where
  (=-=) = eq


newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary



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


instance Monad List  where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a (la)) f = flatMap f (Cons a (la))


j :: Monad m => m (m a) -> m a
j m = join m

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = fmap f ma

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb =  f <$> ma <*> mb 

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

atomlbtomlb :: Monad m => (a -> m b) -> a -> m [b] -> m [b]
atomlbtomlb mfa a mlb  = (\b -> b : [] ) <$> mfa a >>= \lbn -> (\lbo -> lbn ++ lbo) <$> mlb 

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh la mfa = foldr (atomlbtomlb mfa) (return []) la

fliptype :: (Monad m) => [m a] -> m [a]
fliptype lma = meh lma id

main :: IO ()
main = do
  let trigger = undefined :: Nope  (Int,String,Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $  monad trigger
  let trigger = undefined :: PhEither Int (Int,String,Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $  monad trigger
  let trigger = undefined :: Identity (Int,String,Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $  monad trigger
  print "test List Monad"
  let trigger = undefined :: List (Int,String,Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $  monad trigger
