module Shared.ValueObjects.NonZeroSpec (spec) where

import Shared.ValueObjects.NonZero (NonZero (UnsafeNonZero), NonZeroError (IllegalZero), mkNonZero)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "construction" $ do
    describe "mkNonZero" $ do
      it "returns IllegalZero if given zero" $
        do
          mkNonZero (0 :: Int) `shouldBe` Left IllegalZero

      it "returns NonZero if given non-zero" $
        property $
          \(x :: Int) -> x /= 0 ==> mkNonZero x `shouldBe` Right (UnsafeNonZero x)
