module PiggyBalance.Entities.PiggySpec (spec) where

import Data.Aeson (decode, encode)
import PiggyBalance.Entities.Piggy (Piggy)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "parseJSON" $ do
      it "parseJSON can parse the output of toJSON" $
        property $
          \p -> (decode (encode p) :: Maybe Piggy) `shouldBe` Just p
