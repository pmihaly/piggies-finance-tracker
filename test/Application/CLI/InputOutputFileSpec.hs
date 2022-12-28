module Application.CLI.InputOutputFileSpec (spec) where

import Application.CLI.InputOutputFile (InputOutputFile)
import Data.Aeson (decode, encode)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "InputOutputFile" $ do
    describe "introduction" $ do
      describe "parseJSON" $ do
        it "parseJSON can parse the output of toJSON" $
          property $
            \p -> (decode (encode p) :: Maybe InputOutputFile) `shouldBe` Just p
