-- |
module InstancesOfFunc where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

newtype Identity a =
  Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

data Pair a =
  Pair a
       a
  deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

type IntToInt = Fun Int Int

type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

type IdentityIntToIdentityInt = Fun (Identity Int) (Identity Int)

type IdentityFC
   = [Identity Int] -> IdentityIntToIdentityInt -> IdentityIntToIdentityInt -> Bool

main :: IO ()
main = do
  let li x = functorCompose (+ 1) (* 2) (x :: Identity Int)
  quickCheck $ \x -> functorIdentity (x :: Identity [Int])
  verboseCheck $ \x -> functorIdentity (x :: Identity [Int])
  quickCheck li
  quickCheck (functorCompose' :: IntFC)
  -- quickCheck (functorCompose' :: IdentityIntToIdentityInt)
  -- verboseCheck li
