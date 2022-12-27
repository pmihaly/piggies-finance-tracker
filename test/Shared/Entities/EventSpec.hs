module Shared.Entities.EventSpec (spec) where

import Data.Aeson (decode, encode)
import Shared.Entities.Event.Event (Event)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Event" $ do
    describe "introduction" $ do
      describe "parseJSON" $ do
        it "parseJSON can parse the output of toJSON" $
          property $
            \e -> (decode (encode e) :: Maybe Event) `shouldBe` Just e
