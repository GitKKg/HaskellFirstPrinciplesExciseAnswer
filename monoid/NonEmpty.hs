
-- |
import           Data.Semigroup
import           GHC.IO.Encoding
import           System.IO

import           Test.QuickCheck
data NonEmpty a =
  a :| [a]
  deriving (Eq, Ord, Show)

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Show ,Eq)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity a'  = Identity (a <>a')

--instance Eq a => Eq (Identity a) where
  -- Identity a == Identity b = a == b

-- instance Show a => Show (Identity a) where
  -- show (Indentity a) = "Identity" + (show a)

instance (Arbitrary a ,Semigroup a ) => Arbitrary (Identity a) where
   arbitrary = do
   a <- arbitrary
   return (Identity a)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

data Two a b =
  Two a b deriving (Show , Eq)

instance (Semigroup a ,Semigroup b) => Semigroup ( Two a b) where
  Two a b <> Two a' b' = Two ( a <> a') ( b <> b')
type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

newtype BoolConj =
  BoolConj Bool deriving (Show , Eq)

instance Semigroup  BoolConj  where
  BoolConj True <> (BoolConj False) = BoolConj False
  BoolConj True <> _ =  BoolConj True
  BoolConj False <> (BoolConj True) = BoolConj False
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary =   BoolConj <$> (arbitrary :: Gen Bool)

-- also works
{-
instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary :: Gen Bool
    return (BoolConj a)
-}

type BoolConjAssoc = BoolConj  -> BoolConj  -> BoolConj  -> Bool

data Or a b =
  Fst a
  | Snd b deriving (Eq , Show)

instance Semigroup (Or a b) where
  Fst a <> (Fst b) = Fst b
  Fst a <> (Snd b) = Snd b
  Snd a <> (Fst b) = Snd a
  Snd a <> _ = Snd a

instance (Arbitrary a, Arbitrary b) =>  Arbitrary (Or a b) where
  arbitrary =frequency[(1,Fst <$> arbitrary) , (1,Snd <$> arbitrary) ]

type OrAssoc a b = Or a b  -> (Or a b) -> (Or a b) -> Bool

newtype Combine a b =
  Combine {unCombine :: (a->b)}

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> (Combine g) = Combine (f <> g)

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  -- arbitrary = Combine 

newtype Comp a =
  Comp {unComp :: (a->a)}

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> (Comp g) = Comp (f<>g)

data Validation a b =
  Failure a | Success b
  deriving (Eq , Show)

instance Semigroup a => Semigroup (Validation a b ) where
  Main.Failure a <> Main.Failure a' = Main.Failure (a <> a')
  Main.Success b <> Main.Success b' = Main.Success b
  Main.Success a <> Main.Failure a' = Main.Failure a'
  Main.Failure a <> _ = Main.Failure a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b ) where
  arbitrary = frequency[(1,Main.Success <$> arbitrary), (1,Main.Failure <$> arbitrary)]

type ValidationAssoc a b = Validation a b -> (Validation a b) -> (Validation a b) ->Bool
  
  
  


main :: IO ()
main = do
  --setLocaleEncoding utf8
  -- setConsoleOutputCP 65001
--  hSetEncoding stdout utf8
  putStrLn "→"
  quickCheck (semigroupAssoc :: TrivialAssoc)
  verboseCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: (IdentityAssoc [Char]))
  quickCheck (semigroupAssoc :: (IdentityAssoc [Integer]))
  quickCheck (semigroupAssoc :: (TwoAssoc [Integer] [Char]))
  quickCheck (semigroupAssoc :: BoolConjAssoc )
  verboseCheck (semigroupAssoc :: (OrAssoc String [Integer]))
  verboseCheck (semigroupAssoc :: (ValidationAssoc [Integer] [Char]) )
  
