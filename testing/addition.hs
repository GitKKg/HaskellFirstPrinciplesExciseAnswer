module Addition  where

import Test.Hspec
import Test.QuickCheck
main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1+1 is a greater than 1" $ do
      (1+1)>1 `shouldBe` True
    it "x+1 is always greater than x" $ do
      property $ \x -> x+1 > (x::Int)


genBool :: Gen Bool
genBool = choose (False , True)

genBool' :: Gen Bool
genBool' = elements [False ,True]

genOrdering :: Gen Ordering
genOrdering = elements [LT,EQ,GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x+1 > x
runQc :: IO ()
runQc = quickCheck prop_additionGreater
