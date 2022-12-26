{-# LANGUAGE ImportQualifiedPost #-}

module Application.Shared.State (State (..)) where

import Data.HashSet qualified as Set
import PiggyBalance.PiggyBalances (PiggyBalances)
import Shared.Entities.Event.Event (Event)
import Shared.ValueObjects.Id (Id)
import Test.QuickCheck (Arbitrary (arbitrary))

data State = State
  { piggyBalances :: PiggyBalances,
    appliedEvents :: Set.HashSet (Id Event)
  }
  deriving (Show, Eq)

instance Arbitrary State where
  arbitrary = do
    piggyBalances <- arbitrary
    appliedEvents <- Set.fromList <$> arbitrary
    pure (State piggyBalances appliedEvents)
