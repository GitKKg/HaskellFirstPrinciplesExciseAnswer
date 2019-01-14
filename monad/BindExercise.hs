-- | 

module Haskell.Applicative.Monad.BindExercise where

import Control.Monad

bind :: Monad m => (a -> m b) -> m a -> m b
bind  f ma = join $ fmap f ma

addOne x = [x,1]

array = [4,5,6]

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x ,x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x ,x*x]
    else []

twiceWhenEvenM :: [Integer] -> [Integer]
twiceWhenEvenM xs = do
  xs >>=
    \x ->
      if even x
      then [x*x ,x*x]
      else []


data Cow = Cow {
  name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i+1)
  else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a,b,c)

main :: IO ()
main = do
  print $ bind addOne array
