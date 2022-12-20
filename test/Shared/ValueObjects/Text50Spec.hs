{-# LANGUAGE OverloadedStrings #-}

module Shared.ValueObjects.Text50Spec (spec) where

import Data.Text qualified as T
import Shared.ValueObjects.Text50 (Text50 (UnsafeText50, unText50), Text50Error (..), mkText50)
import Test.Hspec

spec :: Spec
spec = do
  describe "construction" $ do
    describe "mkText50" $ do
      it "returns TooShort if the input is empty" $
        do
          mkText50 "" `shouldBe` Left TooShort

      it "returns TooShort if the input is more than 50 characters" $
        do
          mkText50 (T.pack $ take 51 ['A' ..]) `shouldBe` Left TooLong

      it "constructs the string with medium length input" $
        do
          mkText50 "some-text" `shouldBe` Right (UnsafeText50 "some-text")

  describe "elimination" $ do
    it "can be converted to T.Text using unText50" $
      do unText50 (UnsafeText50 "some-text") `shouldBe` "some-text"

    it "can be converted to String using show" $
      do show (UnsafeText50 "some-text") `shouldBe` ("\"some-text\"" :: String)
