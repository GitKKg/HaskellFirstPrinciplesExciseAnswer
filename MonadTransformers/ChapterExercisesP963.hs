-- | 

module Haskell.FirstPrinciples.MonadTransformers.ChapterExercisesP963 where


import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Control.Monad.Trans.State
import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans
-- newtype Reader r a =
   -- Reader {runReader :: r -> a }
rDEC :: Num a => Reader a a
-- rDEC = reader $ \a -> a-1
rDEC = reader $ flip (-) 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
  liftIO $ putStrLn $  "Hi: " ++ show a
  return $ a+1


sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  liftIO $ putStrLn $ "Hi: " ++ show s
  return (show s ,s+1)
