-- | 
{-#LANGUAGE InstanceSigs#-}
module Haskell.FirstPrinciples.Reader.ReaderMonad where

import Control.Monad

newtype Reader r a =
  Reader { runReader :: r -> a }

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


instance Functor (Reader r) where
  fmap f (Reader ra) =
    Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  (<*>) :: Reader r (a -> b)
       -> Reader r a
       -> Reader r b
  (<*>) (Reader rab) (Reader ra)  = Reader $ \r -> (rab r) (ra r)


instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> (runReader . aRb . ra) r r


getDogRM :: Reader Person Dog
getDogRM = Reader $ \r -> Dog (dogName r) (address r)
