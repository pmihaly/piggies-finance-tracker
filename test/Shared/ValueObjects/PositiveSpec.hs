module Shared.ValueObjects.PositiveSpec (spec) where

import Data.Aeson (decode, encode)
import Shared.ValueObjects.Positive (PositiveError (IllegalNegative), mkPositive, unsafePositive)
import Shared.ValueObjects.Positive qualified as Positive
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "mkPositive" $ do
      it "returns IllegalNegative if given negative" $
        property $
          \(x :: Int) -> x < 0 ==> mkPositive (x :: Int) `shouldBe` Left IllegalNegative

      it "returns Positive if given positive or zero" $
        property $
          \(x :: Int) -> x >= 0 ==> mkPositive x `shouldBe` Right (unsafePositive x)

    describe "parseJSON" $ do
      describe "Int" $ do
        it "returns Nothing if given negative" $
          do (decode "-123" :: Maybe (Positive.Positive Int)) `shouldBe` Nothing

        it "returns Positive if given positive or zero" $
          do (decode "123" :: Maybe (Positive.Positive Int)) `shouldBe` Just (unsafePositive 123)

        it "returns Nothing if given float but tries to parse as int" $
          do (decode "123.45" :: Maybe (Positive.Positive Int)) `shouldBe` Nothing

  describe "elimination" $ do
    it "can be converted to JSON using toJSON" $
      do encode (unsafePositive (123 :: Int)) `shouldBe` "123"
