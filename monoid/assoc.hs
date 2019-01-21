
import           Control.Monad
import           Data.Semigroup
-- import           GHC.IO.Encoding
import           System.IO
-- import           System.Win32.Console
import           Test.QuickCheck

monoidAssoc :: (Eq m,Semigroup m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type C = [Integer]

monoidLeftIdentity :: (Eq m,Semigroup m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m,Semigroup m,  Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
  Twoo <> _  = Twoo
  Fools <> Fools =Fools
  _ <> Twoo = Twoo

instance Monoid Bull where
  mappend Twoo _ = Twoo
  mappend _ Twoo = Twoo
  mempty  = Fools


type BullMappend = Bull -> Bull -> Bull -> Bool


data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

newtype First' a = First'
  { getFirst' :: Optional a
  } deriving (Eq, Show)

instance Semigroup (First' a) where
   (First' Nada) <> (First' (Only a)) = First' (Only a)
   (First' (Only a)) <> (First' Nada) = First' (Only a)
   (First' a) <>  _                    = First' a

instance Monoid (First' a) where
   (First' Nada) `mappend` (First' (Only a)) = First' (Only a)
   (First' (Only a)) `mappend` (First' Nada) = First' (Only a)
   (First' a)  `mappend`  _
     = First' a
   mempty  = First' Nada
{-
instance Arbitrary (First' a)  where
  arbitrary =
    frequency [(1,return Nada)
              ,(1,return (Only (sample (arbitrary::Gen Int))))]
-}
instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return (First' Nada)), (1, return (First' (Only a)))]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
  -- setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  verboseCheck ( monoidAssoc  :: C -> C -> C -> Bool)
  quickCheck (monoidLeftIdentity :: String -> Bool)
  quickCheck (monoidRightIdentity :: String -> Bool)
  verboseCheck (monoidLeftIdentity :: String -> Bool)
  verboseCheck (monoidRightIdentity :: String -> Bool)
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  verboseCheck (monoidRightIdentity :: FstId)
  verboseCheck (monoidAssoc :: FirstMappend)
  verboseCheck (monoidLeftIdentity :: FstId)
  verboseCheck (monoidRightIdentity :: FstId)
