-- | 

module Haskell.FirstPrinciples.Reader.ReaderPractiseExercises where

import Control.Applicative
import Data.Maybe

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

-- lookup :: Eq a -> [(a,b)] -> Maybe b

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$>  xs <*> ys 

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n  , z' n)

-- uncurry :: (a -> b -> c) -> (a ,b) -> c

summed :: Num c => (c, c) -> c
summed (a,b) = uncurry (+) (a,b)

bolt :: Integer -> Bool
bolt a
 | a > 3  && a <8  = True
 | otherwise = False

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)


foldsequa = foldr (&&) True . sequA

-- sequatos' = fromMaybe 0 $ sequA <$>  s'

-- fromMaybe :: a -> Maybe a -> a
main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs,  ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3),(<8),even] 7
  print $ foldsequa 12
  print $ fromMaybe [False] $ sequA <$> s'
  print $ fromMaybe True $ bolt <$> ys
  print $ bolt <$> z' 2
