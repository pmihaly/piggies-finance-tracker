module Shared.ValueObjects.NonZeroSpec (spec) where

import Data.Aeson (decode, encode)
import Shared.ValueObjects.NonZero (NonZero (UnsafeNonZero), NonZeroError (IllegalZero), mkNonZero)
import Shared.ValueObjects.NonZero qualified as NonZero
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "mkNonZero" $ do
      it "returns IllegalZero if given zero" $
        do mkNonZero (0 :: Int) `shouldBe` Left IllegalZero

      it "returns NonZero if given non-zero" $
        property $
          \(x :: Int) -> x /= 0 ==> mkNonZero x `shouldBe` Right (UnsafeNonZero x)

    describe "parseJSON" $ do
      describe "Int" $ do
        it "returns Nothing if given zero" $
          do (decode "0" :: Maybe (NonZero.NonZero Int)) `shouldBe` Nothing

        it "returns NonZero if given non-zero" $
          do (decode "123" :: Maybe (NonZero.NonZero Int)) `shouldBe` Just (UnsafeNonZero 123)

        it "returns Nothing if given float but tries to parse as int" $
          do (decode "123.45" :: Maybe (NonZero.NonZero Int)) `shouldBe` Nothing

      describe "RealFrac" $ do
        it "returns Nothing if given zero" $
          do (decode "0.00" :: Maybe (NonZero.NonZero Double)) `shouldBe` Nothing

        it "returns NonZero if given non-zero" $
          do (decode "123.45" :: Maybe (NonZero.NonZero Double)) `shouldBe` Just (UnsafeNonZero 123.45)

  describe "elimination" $ do
    it "can be converted to JSON using toJSON" $
      do encode (UnsafeNonZero (123 :: Int)) `shouldBe` "123"
