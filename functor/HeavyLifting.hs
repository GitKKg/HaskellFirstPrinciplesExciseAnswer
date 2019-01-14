-- | 

module Haskell.Functor.HeavyLifting where

a = (+1) <$>   read "[1]" :: [Int]

b = (fmap.fmap) (++ "lol") (Just ["Hi", "Hello"])

c = fmap  (*2) (\x -> x-2)

d = ((return '1' ++).show) <$> (\x -> [x,1..3])

e :: IO Integer
e = let ioi = 1
        changed = readIO ("123" ++ (show ioi)) :: IO Integer
    in fmap  (*3) changed
