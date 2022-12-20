{-# LANGUAGE ImportQualifiedPost #-}

module Shared.ValueObjects.Text500Spec (spec) where

import Data.Text qualified as T
import Shared.ValueObjects.Text500 (Text500 (UnsafeText500, unText500), Text500Error (..), mkText500)
import Test.Hspec

spec :: Spec
spec = do
  describe "construction" $ do
    describe "mkText500" $ do
      it "returns TooShort if the input is empty" $
        do
          mkText500 "" `shouldBe` Left TooShort

      it "returns TooShort if the input is more than 500 characters" $
        do
          mkText500 (T.pack $ take 501 ['A' ..]) `shouldBe` Left TooLong

      it "constructs the string with medium length input" $
        do
          mkText500 "some-text" `shouldBe` Right (UnsafeText500 "some-text")

  describe "elimination" $ do
    it "can be converted to T.Text using unText500" $
      do unText500 (UnsafeText500 "some-text") `shouldBe` "some-text"

    it "can be converted to String using show" $
      do show (UnsafeText500 "some-text") `shouldBe` ("\"some-text\"" :: String)
