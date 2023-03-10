{-# LANGUAGE ImportQualifiedPost #-}

module Shared.ValueObjects.Text500Spec (spec) where

import Data.Aeson (decode, encode)
import Data.Text qualified as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import Shared.ValueObjects.Text500 (Text500 (unText500), Text500Error (..), mkText500, unsafeText500)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "mkText500" $ do
      it "returns TooShort if the input is empty" $
        do
          mkText500 "" `shouldBe` Left TooShort

      it "returns TooLong if the input is more than 500 characters" $
        do
          mkText500 (T.pack $ Prelude.take 501 ['A' ..]) `shouldBe` Left TooLong

      it "constructs the string with medium length input" $
        do
          mkText500 "some-text" `shouldBe` Right (unsafeText500 "some-text")

    describe "parseJSON" $ do
      it "returns Nothing if input is empty" $
        do (decode "\"\"" :: Maybe Text500) `shouldBe` Nothing

      it "returns Nothing if the input is more than 500 characters" $
        do (decode ("\"" <> TL.encodeUtf8 (TL.pack $ Prelude.take 501 ['A' ..]) <> "\"") :: Maybe Text500) `shouldBe` Nothing

      it "constructs the string with medium length input" $
        do (decode "\"some-text\"" :: Maybe Text500) `shouldBe` Just (unsafeText500 "some-text")

      it "parseJSON can parse the output of toJSON" $
        property $
          \t -> Just t `shouldBe` (decode (encode t) :: Maybe Text500)

  describe "elimination" $ do
    it "can be converted to T.Text using unText500" $
      do unText500 (unsafeText500 "some-text") `shouldBe` "some-text"

    it "can be converted to String using show" $
      do show (unsafeText500 "some-text") `shouldBe` "\"some-text\""

    it "can be converted to JSON using toJSON" $
      do encode (unsafeText500 "some-text") `shouldBe` "\"some-text\""
