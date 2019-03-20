-- | 

module Haskell.FirstPrinciples.Reader.ExerciseP806 where

newtype Reader r a =
  Reader {runReader :: r -> a}

ask :: Reader a a
ask = Reader id
