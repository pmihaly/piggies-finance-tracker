module Shared.ValueObjects.AvailableSpec (spec) where

import PiggyBalance.ValueObjects.Balance (unsafeBalance)
import Shared.ValueObjects.Available (AvailableError (..), mkAvailableMoney, unmkAvailableMoney, unsafeAvailable)
import Shared.ValueObjects.Money (unsafeMoney)
import Shared.ValueObjects.NonZero (unNonZero, unsafeNonZero)
import Shared.ValueObjects.Positive (unPositive, unsafePositive)
import Test.Hspec

spec :: Spec
spec = do
  describe "Available Money" $ do
    describe "introduction" $ do
      describe "mkAvailableMoney" $ do
        it "should return NotEnoughMoney on insufficient funds" $ do
          let amount = unsafeNonZero $ unsafePositive $ unsafeMoney 1000.0
          let balance = unsafeBalance $ unsafeMoney 500.0
          let errorMessageMoney = unPositive $ unNonZero amount

          mkAvailableMoney amount balance `shouldBe` Left (NotEnoughMoney errorMessageMoney)

        it "should return an Available NonZero Positive Money on sufficient funds" $ do
          let amount = unsafeNonZero $ unsafePositive $ unsafeMoney 1000.0
          let balance = unsafeBalance $ unsafeMoney 1000.0
          let expectedAvailableMoney = unsafeAvailable amount

          mkAvailableMoney amount balance `shouldBe` Right expectedAvailableMoney

    describe "elimination" $ do
      describe "unmkAvailableMoney" $ do
        it "should return NotEnoughMoney on insufficient funds" $ do
          let amount = unsafeNonZero $ unsafePositive $ unsafeMoney 1000.0
          let availableAmount = unsafeAvailable amount
          let balance = unsafeBalance $ unsafeMoney 500.0

          let errorMessageMoney = unPositive $ unNonZero amount

          unmkAvailableMoney availableAmount balance `shouldBe` Left (NotEnoughMoney errorMessageMoney)

        it "should return an Available NonZero Positive Money on sufficient funds" $ do
          let amount = unsafeNonZero $ unsafePositive $ unsafeMoney 1000.0
          let availableAmount = unsafeAvailable amount
          let balance = unsafeBalance $ unsafeMoney 1000.0

          unmkAvailableMoney availableAmount balance `shouldBe` Right amount
