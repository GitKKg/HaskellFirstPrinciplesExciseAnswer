-- | 

module Haskell.FirstPrinciples.State.FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State

import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise       = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []
  -- let dlist = execState (mapM_ addResult list) DL.empty
  -- in DL.apply dlist []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)
  
addResultReverse n = do
  xs <- get
  let result = fizzBuzz n
  put ( result :[] ++ xs)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo s e =
   execState (mapM_ addResultReverse [s..e]) []
  


main :: IO ()
main = do
  -- mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
  -- mapM_ putStrLn $ fizzbuzzList [1..100]
  -- mapM_ (putStrLn . fizzBuzz) [1..100]
  mapM_ putStrLn $ fizzbuzzFromTo 1 100 --1
  
