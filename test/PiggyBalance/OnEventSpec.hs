{-# LANGUAGE ImportQualifiedPost #-}

module PiggyBalance.OnEventSpec (spec) where

import Data.HashMap.Strict qualified as Map
import PiggyBalance.Entities.Piggy (PiggyError (InsufficientFunds), unsafePiggy)
import PiggyBalance.OnEvent qualified as PiggyBalance
import PiggyBalance.ValueObjects.Balance (unsafeBalance)
import PiggyBalance.ValueObjects.PiggyBalanceError (PiggyBalanceError (..))
import Shared.Entities.Event.Event (Event (..))
import Shared.ValueObjects.Id (unsafeId)
import Shared.ValueObjects.MaybeNotAvailable (MaybeNotAvailable (..), MaybeNotAvailableError (..))
import Shared.ValueObjects.Money (unsafeMoney)
import Shared.ValueObjects.NonZero (unsafeNonZero)
import Shared.ValueObjects.Positive (unsafePositive)
import Shared.ValueObjects.Text50 (unsafeText50)
import Test.Hspec

spec :: Spec
spec =
  describe "onEvent" $ do
    describe "AddedToPiggy" $ do
      it "should return PiggyNotFound if event references not existing piggy" $
        do
          let notExistingPiggyId = unsafeId (unsafeText50 "not-existing-piggy-id")
          let notReferencedPiggyId = unsafeId (unsafeText50 "not-referenced-piggy-id")
          let balances = Map.singleton notReferencedPiggyId (unsafePiggy notReferencedPiggyId (unsafeBalance (unsafeMoney 1000)))
          let event = AddedToPiggy (unsafeId (unsafeText50 "event-id")) notExistingPiggyId (unsafeNonZero (unsafePositive (unsafeMoney 1000)))

          PiggyBalance.onEvent balances event `shouldBe` Left (PiggyNotFound notExistingPiggyId)

      it "should add deposited amount to the balance" $
        do
          let piggyId = unsafeId (unsafeText50 "not-existing-piggy-id")
          let balances = Map.singleton piggyId (unsafePiggy piggyId (unsafeBalance (unsafeMoney 1000)))
          let event = AddedToPiggy (unsafeId (unsafeText50 "event-id")) piggyId (unsafeNonZero (unsafePositive (unsafeMoney 1000)))

          PiggyBalance.onEvent balances event `shouldBe` Right (Map.singleton piggyId (unsafePiggy piggyId (unsafeBalance (unsafeMoney 2000))))

    describe "TakenFromPiggy" $ do
      it "should return PiggyNotFound if event references not existing piggy" $
        do
          let notExistingPiggyId = unsafeId (unsafeText50 "not-existing-piggy-id")
          let notReferencedPiggyId = unsafeId (unsafeText50 "not-referenced-piggy-id")
          let balances = Map.singleton notReferencedPiggyId (unsafePiggy notReferencedPiggyId (unsafeBalance (unsafeMoney 1000)))
          let event = TakenFromPiggy (unsafeId (unsafeText50 "event-id")) notExistingPiggyId (MaybeNotAvailable (unsafeNonZero (unsafePositive (unsafeMoney 1000))))

          PiggyBalance.onEvent balances event `shouldBe` Left (PiggyNotFound notExistingPiggyId)

      it "should return PiggyHasNotEnoughMoney if trying to withdraw more what is available" $
        do
          let piggyId = unsafeId (unsafeText50 "some-piggy-id")
          let balances = Map.singleton piggyId (unsafePiggy piggyId (unsafeBalance (unsafeMoney 1000)))
          let event = TakenFromPiggy (unsafeId (unsafeText50 "event-id")) piggyId (MaybeNotAvailable (unsafeNonZero (unsafePositive (unsafeMoney 2000))))

          PiggyBalance.onEvent balances event `shouldBe` Left (PiggyHasNotEnoughMoney $ InsufficientFunds $ NotEnoughMoney $ unsafeMoney 2000)

      it "should subtract the withdrawn amount from the balance" $
        do
          let piggyId = unsafeId (unsafeText50 "some-piggy-id")
          let balances = Map.singleton piggyId (unsafePiggy piggyId (unsafeBalance (unsafeMoney 500)))
          let event = TakenFromPiggy (unsafeId (unsafeText50 "event-id")) piggyId (MaybeNotAvailable (unsafeNonZero (unsafePositive (unsafeMoney 300))))

          PiggyBalance.onEvent balances event `shouldBe` Right (Map.singleton piggyId $ unsafePiggy piggyId $ unsafeBalance $ unsafeMoney 200)

    describe "MovedBetweenPiggies" $ do
      it "should return PiggyNotFound if event references not existing piggy (sender)" $
        do
          let notExistingSender = unsafeId (unsafeText50 "not-existing-sender")
          let receiver = unsafeId (unsafeText50 "receiver")
          let balances = Map.singleton receiver (unsafePiggy receiver (unsafeBalance (unsafeMoney 1000)))
          let event = MovedBetweenPiggies (unsafeId (unsafeText50 "event-id")) notExistingSender receiver (MaybeNotAvailable (unsafeNonZero (unsafePositive (unsafeMoney 1000))))

          PiggyBalance.onEvent balances event `shouldBe` Left (PiggyNotFound notExistingSender)

      it "should return PiggyNotFound if event references not existing piggy (receiver)" $
        do
          let sender = unsafeId (unsafeText50 "sender")
          let notExistingReceiver = unsafeId (unsafeText50 "not-existing-receiver")
          let balances = Map.singleton sender (unsafePiggy sender (unsafeBalance (unsafeMoney 1000)))
          let event = MovedBetweenPiggies (unsafeId (unsafeText50 "event-id")) sender notExistingReceiver (MaybeNotAvailable (unsafeNonZero (unsafePositive (unsafeMoney 1000))))

          PiggyBalance.onEvent balances event `shouldBe` Left (PiggyNotFound notExistingReceiver)

      it "should return PiggyHasNotEnoughMoney if trying to withdraw more what is available" $
        do
          let sender = unsafeId (unsafeText50 "sender")
          let receiver = unsafeId (unsafeText50 "receiver")
          let balances = Map.union (Map.singleton sender (unsafePiggy sender (unsafeBalance (unsafeMoney 200)))) (Map.singleton receiver (unsafePiggy receiver (unsafeBalance (unsafeMoney 500))))
          let event = MovedBetweenPiggies (unsafeId (unsafeText50 "event-id")) sender receiver (MaybeNotAvailable (unsafeNonZero (unsafePositive (unsafeMoney 1000))))

          PiggyBalance.onEvent balances event `shouldBe` Left (PiggyHasNotEnoughMoney $ InsufficientFunds $ NotEnoughMoney $ unsafeMoney 1000)

      it "should move amount between the balances" $
        do
          let sender = unsafeId (unsafeText50 "sender")
          let receiver = unsafeId (unsafeText50 "receiver")
          let balances = Map.union (Map.singleton sender (unsafePiggy sender (unsafeBalance (unsafeMoney 500)))) (Map.singleton receiver (unsafePiggy receiver (unsafeBalance (unsafeMoney 500))))
          let event = MovedBetweenPiggies (unsafeId (unsafeText50 "event-id")) sender receiver (MaybeNotAvailable (unsafeNonZero (unsafePositive (unsafeMoney 300))))

          PiggyBalance.onEvent balances event `shouldBe` Right (Map.union (Map.singleton sender (unsafePiggy sender (unsafeBalance (unsafeMoney 200)))) (Map.singleton receiver (unsafePiggy receiver (unsafeBalance (unsafeMoney 800)))))
