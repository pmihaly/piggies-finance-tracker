module Shared.Entities.EventSpec (spec) where

import Data.Aeson (decode, encode)
import Shared.Entities.Event.Event (Event)
import Shared.ValueObjects.ArbitraryEvent (ArbitraryEvent, unArbitraryEvent)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Event" $ do
    describe "introduction" $ do
      describe "parseJSON" $ do
        it "parseJSON can parse the output of toJSON" $
          property $
            \(ae :: ArbitraryEvent) -> 
                let e = unArbitraryEvent ae in
              (decode (encode e) :: Maybe Event) `shouldBe` Just e
