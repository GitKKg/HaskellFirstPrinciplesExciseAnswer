-- | 

module Haskell.FirstPrinciples.Traversable.DoAllTheThings where

import Data.ByteString.Lazy
import Network.Wreq -- proxychains stack install wreq

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
urls :: [String]
urls = ["http://httpbin.com/ip"
       ,"http://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = Prelude.map get urls


traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls

type TI = []

main = do
  let trigger = undefined :: TI (Int , Int , [Int])
  quickBatch (traversable trigger)
