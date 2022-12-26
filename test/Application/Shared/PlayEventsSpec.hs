{-# LANGUAGE ImportQualifiedPost #-}

module Application.Shared.PlayEventsSpec (spec) where

import Application.Shared.PlayEvents (playEvents)
import Application.Shared.State (appliedEvents)
import Control.Category ((>>>))
import Data.HashSet qualified as Set
import Lens.Micro.Platform
import Shared.Entities.Event.Event (getEventId)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  it "should insert eventId to appliedEvents " $
    property $
      \s e -> ((^. appliedEvents) >>> Set.member (getEventId e)) <$> playEvents s [e] `shouldBe` pure True

  describe "idempotence" $ do
    it "should not apply the same event twice" $
      property $
        \s e -> playEvents s [e] `shouldBe` playEvents s [e, e]
