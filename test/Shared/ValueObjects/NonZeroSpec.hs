{-# LANGUAGE ImportQualifiedPost #-}
module Shared.ValueObjects.NonZeroSpec (spec) where

import Data.Aeson (decode, encode)
import Shared.ValueObjects.NonZero (NonZeroError (IllegalZero), mkNonZero, unsafeNonZero)
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
          \(n :: Int) -> n /= 0 ==> mkNonZero n `shouldBe` Right (unsafeNonZero n)

    describe "parseJSON" $ do
      describe "Int" $ do
        it "returns Nothing if given zero" $
          do (decode "0" :: Maybe (NonZero.NonZero Int)) `shouldBe` Nothing

        it "returns NonZero if given non-zero" $
          do (decode "123" :: Maybe (NonZero.NonZero Int)) `shouldBe` Just (unsafeNonZero 123)

        it "returns Nothing if given float but tries to parse as int" $
          do (decode "123.45" :: Maybe (NonZero.NonZero Int)) `shouldBe` Nothing

        it "parseJSON can parse the output of toJSON" $
          property $
            \n -> Just n `shouldBe` (decode (encode n) :: Maybe (NonZero.NonZero Int))

      describe "RealFrac" $ do
        it "returns Nothing if given zero" $
          do (decode "0.00" :: Maybe (NonZero.NonZero Double)) `shouldBe` Nothing

        it "returns NonZero if given non-zero" $
          do (decode "123.45" :: Maybe (NonZero.NonZero Double)) `shouldBe` Just (unsafeNonZero 123.45)

        it "parseJSON can parse the output of toJSON" $
          property $
            \n -> Just n `shouldBe` (decode (encode n) :: Maybe (NonZero.NonZero Double))

  describe "elimination" $ do
    it "can be converted to JSON using toJSON" $
      do encode (unsafeNonZero (123 :: Int)) `shouldBe` "123"
