module Shared.ValueObjects.PositiveSpec (spec) where

import Shared.ValueObjects.Positive (Positive (UnsafePositive), PositiveError (IllegalNegative), mkPositive)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "construction" $ do
    describe "mkPositive" $ do
      it "returns IllegalNegative if given negative" $
        property $
          \(x :: Int) -> x < 0 ==> mkPositive (x :: Int) `shouldBe` Left IllegalNegative

      it "returns Positive if given positive or zero" $
        property $
          \(x :: Int) -> x >= 0 ==> mkPositive x `shouldBe` Right (UnsafePositive x)
