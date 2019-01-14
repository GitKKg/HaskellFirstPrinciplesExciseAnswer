-- | 

module Haskell.Monad.EitherMonad where
import Control.Monad
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b =
  First a
  | Second b
  deriving (Eq , Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap f (First a) = First a

instance Applicative (Sum a) where
  pure b = Second b
  (<*>) (Second f) (Second b) = Second (f b)
  (<*>) (Second f) (First a) = First a
  (<*>) (First a) (First a1) = First a1
  (<*>) (First a) (Second b) = First a
  

instance Monad (Sum a) where
  return = pure
  (>>=) (Second b) f = f b
  (>>=) (First a) f = First a

instance (Arbitrary a ,Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, First <$> arbitrary)
                        ,(1, Second <$> arbitrary)
                        ]
instance (Eq a,Eq b) => EqProp (Sum a b) where
  (=-=) = eq

main :: IO ()
main = do
  let trigger = undefined :: Sum Int (Int, String, Int)  
  quickBatch (applicative trigger)
  quickBatch (monad trigger)  
