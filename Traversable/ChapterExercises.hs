-- | 

module Haskell.FirstPrinciples.Traversable.ChapterExercises where
import Data.Monoid
import Data.Semigroup
import Data.Traversable
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq , Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f ) (Identity a) = Identity (f a) -- just same with fmap 

instance Foldable Identity  where
  foldr f b (Identity a) = f a b 

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

newtype Constant a b =
  Constant {getConstant :: a}
  deriving (Eq , Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a


instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  (<*>) (Constant a) (Constant b) = Constant (a `mappend` b)

instance Foldable (Constant a) where
  foldMap _ (Constant a) = mempty

instance Traversable  (Constant a) where
  traverse f (Constant a) = pure $ Constant a 

data Optional a =
  Nada
  | Yep a
  deriving (Eq , Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Applicative Optional where
  pure  = Yep
  (<*>) _ Nada = Nada
  (<*>) Nada _ = Nada
  (<*>) (Yep f) (Yep da) = Yep (f da)

instance Foldable Optional where
  foldMap f (Yep a) = f a
  foldMap _ Nada = mempty

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a  

instance Arbitrary a =>  Arbitrary (Optional a) where
  arbitrary =
    frequency [(1, return Nada),
              (1,  Yep <$> arbitrary )
              ]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

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
  --trait of thought: first iter data ,then iter function
  -- Cons f lf <*> Cons da ldata = (f <$> (Cons da ldata)) `append` ( lf <*> Cons da ldata)
  Cons f lf <*> Cons da ldata =
    flatMap (($ (Cons da ldata)) (flip fmap)) (Cons f lf)

-- data List a
  -- = Nil
  -- | Cons a
         -- (List a)
  -- deriving (Eq, Show)
instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons a la) =  f a `mappend` foldMap f  la

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse fa (Cons a la) =   Cons <$> (fa a) <*> (traverse fa la)
  
-- pure $ flatMap ( ($ Nil ) . Cons <$>) (fmap fa  (Cons a la))

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = flip repeat' 5 <$> arbitrary  

repeat' :: a ->Int-> List a
repeat'  a 0 = Cons a Nil
repeat' a n = as
  where as = Cons a (repeat' a (n-1))

instance Eq a => EqProp (List a) where
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

instance Foldable (Three a b) where
  foldr f b0 (Three a b c) = f c b0

instance Traversable (Three a b) where
  traverse fb (Three a b c) = Three a b <$> (fb c)

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

instance Foldable (Three' a) where
  foldr f b0 (Three' a b1 b2) = foldr f b0 [b1,b2]

  
instance Traversable (Three' a) where
  traverse fa (Three' a b1 b2) = Three' a <$> (fa b1) <*> fa b2
  

data S n a = S (n a) a

instance Functor n =>  Functor (S n) where
  fmap f (S na1 a2) = S ( f <$> na1) (f a2)

-- instance Monoid  a  => Monoid (S n a) where
  -- mempty = S n mempty
  -- mappend (S n1 a1) (S n2 a2) = S (mappend n1 n2 ) (mappend a1 a2)  

-- instance Monoid n => Applicative (S n ) where
  -- pure a = S (mempty a) a
  -- (<*>) (S (nf1) f2) (S (nd1) d2) =  S (nf1 nd1) (f2 d2)
  

instance Foldable n => Foldable (S n) where
  foldMap fm (S na1 a2) = foldMap fm na1 `mappend` fm a2    

-- instance Foldable (S n) where
  -- foldMap = undefined

instance  Traversable n => Traversable (S n) where
  -- traverse fb (S na1 a2) = S  <$> (traverse fb (na1 )) <*> fb a2
  traverse fb (S na1 a2) = S <$> (traverse fb na1) <*> fb a2

main = do
  let trigger = undefined :: Optional (Int,Int,[Int])
  quickBatch (traversable trigger)
  let trigger = undefined :: List (Int,Int,[Int])
  quickBatch (traversable trigger)
  let trigger = undefined :: Three' Int (Int,Int,[Int])
  print "Test Three' instance Traversable"
  quickBatch (traversable trigger)
