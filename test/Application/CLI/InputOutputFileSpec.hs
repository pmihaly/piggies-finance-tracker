module Application.CLI.InputOutputFileSpec (spec) where

import Data.Aeson (decode, encode)
import Test.Hspec
import Test.QuickCheck
import Application.CLI.InputOutputFile (InputOutputFile)

spec :: Spec
spec = do
  describe "InputOutputFile" $ do
    describe "introduction" $ do
      describe "parseJSON" $ do
        it "parseJSON can parse the output of toJSON" $
          property $
            \p -> (decode (encode p) :: Maybe InputOutputFile) `shouldBe` Just p
