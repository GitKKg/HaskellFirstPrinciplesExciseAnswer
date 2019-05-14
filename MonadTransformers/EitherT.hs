-- | 

module Haskell.FirstPrinciples.MonadTransformers.EitherT where
import Control.Monad
import Data.Either
newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT ((fmap . fmap) f mea)
  
instance Applicative m => Applicative (EitherT e m) where --Either is also applicative
  pure a = EitherT $ pure (Right a)
  f <*> a = EitherT $ fmap (<*>) (runEitherT f) <*> (runEitherT a)  

instance Monad m => Monad (EitherT e m) where
  return = pure
  -- pass ,but lack of Left case,no way to case Left,return what? Nothing?
  {-v>>= f =
    let eithertoa = \e -> case e of Right a -> a
    in
    EitherT $ runEitherT v >>= runEitherT . f . eithertoa
   -}
    
  -- failed, m is a type,we don't know its value constructor...
  {-v >>= f =
    case (fmap .fmap)  f (runEitherT v) of
      m (Right a) -> let eithertoa = \e -> case e of Right a -> a -- m is a type,we don't know its value constructor...
        in
        EitherT $ runEitherT v >>= runEitherT . f . eithertoa
  -}
  v >>= f = EitherT $ do
    ea  <- runEitherT v -- in this context,m is m ,<- removes shell m
    case ea of
      Right a ->  runEitherT (f a)
      Left e -> return (Left e) --block by thougt to give Nothing not keeping same with orginal


swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: Monad m =>
           (a -> m c)
           -> (b -> m c)
           -> EitherT a m b
           -> m c

eitherT amc bmc eamb = do
  ea <- runEitherT eamb -- <- removes m from mea to get ea
  case ea of
    Left a -> amc a
    Right b -> bmc b
