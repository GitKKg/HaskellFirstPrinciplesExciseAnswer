{-# LANGUAGE TemplateHaskell #-}
-- | 

module Haskell.Applicative.Lookups where

import Control.Applicative
import Data.List (elemIndex)
added :: Maybe Integer

added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])


y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer , Integer)
tupled = (,) <$> y <*> z



x :: Maybe Int
x = elemIndex  3 [1,2,3,4,5]

y1 :: Maybe Int
y1 = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed =  max' <$>  x <*> y1

xs = [1,2,3]
ys = [4,5,6]

x2 :: Maybe Integer
x2 = lookup 3 $ zip xs ys

y2 :: Maybe Integer
y2 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$>  ( (,) <$> x2 <*> y2)
