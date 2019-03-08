-- | 

module Haskell.FirstPrinciples.Foldable.Exercise where


import Data.Foldable
import Data.Monoid

sumByfoldr :: (Foldable t ,Num a) => t a ->a
sumByfoldr ta  = foldr (+) 0 ta

sumWithfoldMap :: (Foldable t ,Num a) => t a ->a
sumWithfoldMap ta = getSum $ foldMap Sum ta 

productByfoldr :: (Foldable t ,Num a) => t a -> a
productByfoldr ta = foldr (*) 1 ta

productByfoldMap :: (Foldable t ,Num a) => t a -> a
productByfoldMap ta = getProduct $ foldMap Product ta

elemByfoldMap :: (Foldable t ,Eq a) => a -> t a -> Bool
elemByfoldMap a ta =getAny $ foldMap (Any . (a ==))  ta

elemByfoldr :: (Foldable t , Eq a) => a -> t a -> Bool
elemByfoldr a ta = foldr ( (||) . (a==)) False ta

-- Just remind you the disapperaence of traditional if else and loop in functional language, make code neater, neater means human friendly ,means less error and easy to maintian and debug and understandable
minimumByfoldr :: (Foldable t , Ord a) => t a -> Maybe a
minimumByfoldr ta = foldr (\a -> \b -> fmap (min a) b) Nothing ta

-- minimumByfoldMap :: (Foldable t ,Ord a) => t a -> Maybe a
-- will be ugly

maxmumByfoldr :: (Foldable t , Ord a) => t a -> Maybe a
maxmumByfoldr ta = foldr (\a -> \b -> fmap (max a) b) Nothing ta

nullByfoldr :: (Foldable t,Eq a) => t a -> Bool
-- null ta = foldr ((&&) . (mempty == )) True ta
-- below is implementation of Library described in hoogle, it's just out of the mind!
nullByfoldr ta = foldr (\_ _ -> False) True ta

lengthByfoldr :: (Foldable t) => t a -> Int
-- just study from null
lengthByfoldr ta = foldr (\_ b -> b+1) 0 ta

toListByfoldr :: (Foldable t) => t a -> [a]
toListByfoldr ta = foldr (\a b  -> mappend [a] b) [] ta

foldByfoldMap :: (Foldable t,Monoid m) => t m -> m
foldByfoldMap tm = foldMap id tm


foldMapByfoldr :: (Foldable t , Monoid m) => (a -> m) -> t a -> m
foldMapByfoldr atom ta = foldr (\a b -> mappend (atom a) b) mempty ta 
