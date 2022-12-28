{-# LANGUAGE ImportQualifiedPost #-}

module Application.Shared.PlayEventsSpec (spec) where

import Application.Shared.PlayEvents (playEvents)
import Application.Shared.State (appliedEvents, piggyBalances)
import Control.Category ((>>>))
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Lens.Micro.Platform
import Shared.ValueObjects.ArbitraryEvent (arbitraryEventFromState)
import Shared.Entities.Event.Event (getEventId)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  it "should insert eventId to appliedEvents " $
    property $
      \state ->
        not (Map.null $ state ^. piggyBalances)
          ==> forAll (arbitraryEventFromState state)
          $ \event -> do
            ((^. appliedEvents) >>> Set.member (getEventId event)) <$> playEvents state [event] `shouldBe` pure True

  describe "idempotence" $ do
    it "should not apply the same event twice" $
      property $
        \state ->
          not (Map.null $ state ^. piggyBalances)
            ==> forAll (arbitraryEventFromState state)
            $ \event -> playEvents state [event] `shouldBe` playEvents state [event, event]
