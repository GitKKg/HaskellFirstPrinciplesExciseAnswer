import Test.QuickCheck
import Data.Semigroup

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

-- instance Semigroup (Combine a b) where

indexOf :: [a] -> Int-> a

indexOf xs n
  | n < 0      = (-9999)
  | otherwise =
    foldr
     (\x r k ->
        case k of
          0 -> x
          _ -> r(k-1))
     (const (-9999))
     xs
     n
  
