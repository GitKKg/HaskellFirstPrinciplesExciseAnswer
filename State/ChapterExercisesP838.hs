{-# LANGUAGE InstanceSigs #-}
-- | 

module Haskell.FirstPrinciples.State.ChapterExercisesP838 where


import Control.Monad
-- import Control.Monad.Trans.State

newtype State s a =
  State {runState :: s -> (a,s)}

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ \s1 -> (() , s)

exec :: State s a -> s -> s
exec (State sa) s =
  snd $ sa s

eval :: State s a -> s -> a
eval (State sa) s =
  fst $ sa s

modify :: (s -> s) -> State s ()
modify f =
  State $ \s -> (() , f s)

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  -- pass but not smart , for g are called twice
  -- fmap f (State g) = State $ \s -> (f . fst $ g s, snd $  g s)
  fmap f (State g) = State $ \s -> (,) <$> f . fst <*> snd $  g s  

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State f) <*> (State g) =
    -- 1.pass, but generated s many times
    -- State $ \s -> ( (fst $ f s) (fst $ g s) ,  snd $ g s)
    -- 2.pass, but still generated s many times
    -- State $ \s -> ( (fst $ f s) (fst $ g s) ,  snd $ f s)
    -- 3.pass but still generated s 3 times
    -- State $ \s -> runState (fmap (fst $ f s) (State g)) s
    -- 4. g s and f s are both only called once at least explicitly
    -- ,but use a of gs as new a is right? just think it as matrix multipication
    State $ \s -> (,) <$> (fst $ f s) . fst <*> snd $ g s

instance Monad (State s) where
  return a = State $ \s -> (a,s)
  (State f) >>= g = State $ \s -> runState (g . fst $ f s) s
  (State f) >> (State g) = State $ \s ->  g (snd $ f s)

main :: IO ()
main = do
  print $ runState get "curryIsAmaze"
  print $ runState (put "blah") "woot"
  print $ exec (put "wilma") "daphne"
  print $ exec get "scooby papu"
  print $ eval get "bunnicula"
  print $ eval get "stake a bunny"
  print $ runState (modify (+1)) 0
  print $ runState (modify (+1) >> modify (+1)) 0
