module PiggyBalance.OnEventSpec (spec) where

import Application.Shared.PlayEvents (playEvents)
import Application.Shared.State (piggyBalances)
import Control.Monad (replicateM)
import Data.HashMap.Strict qualified as Map
import Lens.Micro.Platform
import PiggyBalance.Entities.Piggy (unsafePiggy)
import PiggyBalance.OnEvent qualified as PiggyBalance
import PiggyBalance.ValueObjects.Balance (unsafeBalance)
import PiggyBalance.ValueObjects.PiggyBalanceError (PiggyBalanceError (..))
import Shared.Entities.Event.Event (Event (..), arbitraryAddedToPiggy)
import Shared.ValueObjects.Id (unsafeId)
import Shared.ValueObjects.Money (unsafeMoney)
import Shared.ValueObjects.NonZero (unsafeNonZero)
import Shared.ValueObjects.Positive (unsafePositive)
import Shared.ValueObjects.Text50 (unsafeText50)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "onAddedToPiggy" $ do
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