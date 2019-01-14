-- | 
import Test.QuickCheck
import Data.Semigroup


data NonEmpty a =a :| [a]
  deriving (Eq,Ord,Show)

data Trivial =Trivial deriving (Eq,Show)

instance Semigroup Trivial where
  _ <> _= Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc::(Eq m,Semigroup m) => m->m->m->Bool
semigroupAssoc a b c =(a<>(b<>c))==((a<>b)<>c)
type TrivialAssoc = Trivial ->Trivial->Trivial ->Bool
newtype Identity a = Identity a 
instance Semigroup (Identity a) where
  Identity a <>_ = Identity a

instance Eq (Identity a) where
  Identity a == _ = True
instance Show a => Show (Identity a) 

instance Arbitrary a =>  Arbitrary  ( Identity a) where
  arbitrary = do
    a<- arbitrary
    return (Identity a)

type IdentityAssoc a    = (Identity a) ->(Identity a)-> (Identity a)->Bool


main ::IO()
main = do
  quickCheck(semigroupAssoc::TrivialAssoc)
  verboseCheck(semigroupAssoc::TrivialAssoc)
  quickCheck(semigroupAssoc::(  IdentityAssoc a))
