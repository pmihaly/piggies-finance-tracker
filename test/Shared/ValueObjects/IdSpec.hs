module Shared.ValueObjects.IdSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import Shared.ValueObjects.Id (Id, unsafeId)
import Shared.ValueObjects.Text50 (unsafeText50)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "parseJSON" $ do
      it "returns Nothing if input is empty" $
        do (decode "\"\"" :: Maybe (Id ())) `shouldBe` Nothing

      it "returns Nothing if the input is more than 500 characters" $
        do (decode ("\"" <> TL.encodeUtf8 (TL.pack $ Prelude.take 501 ['A' ..]) <> "\"") :: Maybe (Id ())) `shouldBe` Nothing

      it "constructs the string with medium length input" $
        do (decode "\"some-text\"" :: Maybe (Id ())) `shouldBe` Just (unsafeId (unsafeText50 "some-text"))

      it "parseJSON can parse the output of toJSON" $
        property $
          \i -> Just i `shouldBe` (decode (encode i) :: Maybe (Id ()))

  describe "elimination" $ do
    it "can be converted to JSON using toJSON" $
      do encode (unsafeId (unsafeText50 "some-text")) `shouldBe` "\"some-text\""
