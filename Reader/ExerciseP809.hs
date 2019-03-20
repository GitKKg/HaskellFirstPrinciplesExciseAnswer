-- | 
{-# LANGUAGE InstanceSigs  #-}
module Haskell.FirstPrinciples.Reader.ExerciseP809 where
import Control.Applicative
newtype Reader r a =
  Reader { runReader :: r -> a}
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader ra) =
    Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader  (undefined :: (r -> a))
  (<*>) :: Reader r (a -> b)
       -> Reader r a
       -> Reader r b
  (<*>) (Reader rab) (Reader ra)  = Reader $ \r -> (rab r) (ra r)


newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Adress String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
   } deriving (Eq, Show)

data Dog =
  Dog {
      dogsName :: DogName
      ,dogsAddress :: Address
      } deriving (Eq, Show)

getDogR :: Reader Person Dog
getDogR = Reader (Dog <$> dogName <*> address)

getPerson :: Reader Person Person
getPerson = Reader (Person <$> humanName <*> dogName <*> address)
