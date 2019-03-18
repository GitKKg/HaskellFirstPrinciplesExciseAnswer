-- | 

module Haskell.FirstPrinciples.Reader.ShortExercise where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = rev <$> cap 

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap 

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  a <- rev
  b <- cap
  return (,) a b
  -- below is inspiration for tupleM
  -- fmap ((,) a) (return b)
  

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  a <- rev
  b <- cap
  -- not right, a is not applied
  -- return a >>= return ((,) b)
  -- right,just tricky and ugly
  return ((,) a) >>= (flip fmap) (return b)
  
