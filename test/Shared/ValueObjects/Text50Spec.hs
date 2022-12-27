{-# LANGUAGE ImportQualifiedPost #-}

module Shared.ValueObjects.Text50Spec (spec) where

import Data.Aeson (decode, encode)
import Data.Text qualified as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import Shared.ValueObjects.Text50 (Text50, Text50Error (..), mkText50, unText50, unsafeText50)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "mkText50" $ do
      it "returns TooShort if the input is empty" $
        do
          mkText50 "" `shouldBe` Left TooShort

      it "returns TooLong if the input is more than 50 characters" $
        do
          mkText50 (T.pack $ Prelude.take 51 ['A' ..]) `shouldBe` Left TooLong

      it "constructs the string with medium length input" $
        do
          mkText50 "some-text" `shouldBe` Right (unsafeText50 "some-text")

    describe "parseJSON" $ do
      it "returns Nothing if input is empty" $
        do (decode "\"\"" :: Maybe Text50) `shouldBe` Nothing

      it "returns Nothing if the input is more than 50 characters" $
        do (decode ("\"" <> TL.encodeUtf8 (TL.pack $ Prelude.take 51 ['A' ..]) <> "\"") :: Maybe Text50) `shouldBe` Nothing

      it "constructs the string with medium length input" $
        do (decode "\"some-text\"" :: Maybe Text50) `shouldBe` Just (unsafeText50 "some-text")

      it "parseJSON can parse the output of toJSON" $
        property $
          \t -> Just t `shouldBe` (decode (encode t) :: Maybe Text50)

  describe "elimination" $ do
    it "can be converted to T.Text using unText50" $
      do unText50 (unsafeText50 "some-text") `shouldBe` "some-text"

    it "can be converted to String using show" $
      do show (unsafeText50 "some-text") `shouldBe` "\"some-text\""

    it "can be converted to JSON using toJSON" $
      do encode (unsafeText50 "some-text") `shouldBe` "\"some-text\""
