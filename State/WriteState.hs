{-# LANGUAGE InstanceSigs #-}
-- | 

module Haskell.FirstPrinciples.State.WriteState where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  -- pass but not smart , for g are called twice
  -- fmap f (Moi g) = Moi $ \s -> (f . fst $ g s, snd $  g s)
  fmap f (Moi g) = Moi $ \s -> (,) <$> f . fst <*> snd $  g s  

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) =
    -- 1.pass, but generated s many times
    -- Moi $ \s -> ( (fst $ f s) (fst $ g s) ,  snd $ g s)
    -- 2.pass, but still generated s many times
    -- Moi $ \s -> ( (fst $ f s) (fst $ g s) ,  snd $ f s)
    -- 3.pass but still generated s 3 times
    -- Moi $ \s -> runMoi (fmap (fst $ f s) (Moi g)) s
    -- 4. g s and f s are both only called once at least explicitly
    -- ,but use a of gs as new a is right? just think it as matrix multipication
    Moi $ \s -> (,) <$> (fst $ f s) . fst <*> snd $ g s
    
instance Monad (Moi s) where
  return a = Moi $ \s -> (a,s)
  (Moi f) >>= g = Moi $ \s -> runMoi (g . fst $ f s) s


-- some idea ispired by these exercises, tricky
-- for not only paralell data with data ,but also function with data

f = (+10)
h = (+12)
g = ($ 2)
p = ($) <$> (f .)  <*> g $ h

main :: IO ()
main = do
  print $ runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0
  print p
