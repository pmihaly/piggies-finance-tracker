module Shared.ValueObjects.MaybeNotAvailableSpec (spec) where

import PiggyBalance.ValueObjects.Balance (unsafeBalance)
import Shared.ValueObjects.MaybeNotAvailable (MaybeNotAvailable (MaybeNotAvailable), MaybeNotAvailableError (..), unmkMaybeNotAvailableMoney)
import Shared.ValueObjects.Money (unsafeMoney)
import Shared.ValueObjects.NonZero (unNonZero, unsafeNonZero)
import Shared.ValueObjects.Positive (unPositive, unsafePositive)
import Test.Hspec

spec :: Spec
spec = do
  describe "MaybeNotAvailable Money" $ do
    describe "elimination" $ do
      describe "unmkMaybeNotAvailableMoney" $ do
        it "should return NotEnoughMoney on insufficient funds" $ do
          let amount = unsafeNonZero $ unsafePositive $ unsafeMoney 1000.0
          let maybeNotAvailableAmount = MaybeNotAvailable amount
          let balance = unsafeBalance $ unsafeMoney 500.0

          let errorMessageMoney = unPositive $ unNonZero amount

          unmkMaybeNotAvailableMoney maybeNotAvailableAmount balance `shouldBe` Left (NotEnoughMoney errorMessageMoney)

        it "should return an MaybeNotAvailable NonZero Positive Money on sufficient funds" $ do
          let amount = unsafeNonZero $ unsafePositive $ unsafeMoney 1000.0
          let maybeNotAvailableAmount = MaybeNotAvailable amount
          let balance = unsafeBalance $ unsafeMoney 1000.0

          unmkMaybeNotAvailableMoney maybeNotAvailableAmount balance `shouldBe` Right amount
