module Shared.ValueObjects.EventIdSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import Shared.ValueObjects.EventId (EventId (UnsafeEventId))
import Test.Hspec

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "parseJSON" $ do
      it "returns Nothing if input is empty" $
        do (decode "\"\"" :: Maybe EventId) `shouldBe` Nothing

      it "returns Nothing if the input is more than 500 characters" $
        do (decode ("\"" <> TL.encodeUtf8 (TL.pack $ Prelude.take 501 ['A' ..]) <> "\"") :: Maybe EventId) `shouldBe` Nothing

      it "constructs the string with medium length input" $
        do (decode "\"some-text\"" :: Maybe EventId) `shouldBe` Just (UnsafeEventId "some-text")

  describe "elimination" $ do
    it "can be converted to JSON using toJSON" $
      do encode (UnsafeEventId "some-text") `shouldBe` "\"some-text\""
