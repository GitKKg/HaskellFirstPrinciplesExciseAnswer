-- | 

module Haskell.Applicative.FixerUpper where

import Control.Applicative

d = const <$> Just "Hello" <*> Just "World"

e = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just  [1,2,3]

