{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Application.Shared.State (State (State), appliedEvents, piggyBalances) where

import Data.HashSet qualified as Set
import Lens.Micro.Platform (makeLenses)
import PiggyBalance.PiggyBalances (PiggyBalances)
import Shared.Entities.Event.Event (Event)
import Shared.ValueObjects.Id (Id)
import Test.QuickCheck (Arbitrary (arbitrary))

data State = State
  { _piggyBalances :: PiggyBalances,
    _appliedEvents :: Set.HashSet (Id Event)
  }
  deriving (Show, Eq)

instance Arbitrary State where
  arbitrary = do
    piggyBalances <- arbitrary
    appliedEvents <- Set.fromList <$> arbitrary
    pure (State piggyBalances appliedEvents)

makeLenses ''State
