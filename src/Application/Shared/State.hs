{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Application.Shared.State (State (State), appliedEvents, piggyBalances) where

import Data.Aeson (ToJSON)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import GHC.Generics (Generic)
import Lens.Micro.Platform (makeLenses)
import PiggyBalance.PiggyBalances (PiggyBalances)
import Shared.Entities.Event.Event (Event)
import Shared.ValueObjects.Id (Id)
import Test.QuickCheck (Arbitrary (arbitrary))

data State = State
  { _piggyBalances :: PiggyBalances,
    _appliedEvents :: Set.HashSet (Id Event)
  }
  deriving (Show, Eq, Generic)

instance ToJSON State

instance Arbitrary State where
  arbitrary = do
    piggyBalances <- Map.fromList <$> arbitrary
    appliedEvents <- Set.fromList <$> arbitrary
    pure (State piggyBalances appliedEvents)

makeLenses ''State
