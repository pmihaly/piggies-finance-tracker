{-# LANGUAGE ImportQualifiedPost #-}

module Application.Shared.State (State (..)) where

import Data.HashSet qualified as Set
import PiggyBalance.PiggyBalances (PiggyBalances)
import Shared.Entities.Event.Event (Event)
import Shared.ValueObjects.Id (Id)

data State = State
  { piggyBalances :: PiggyBalances,
    appliedEvents :: Set.HashSet (Id Event)
  }
  deriving (Show, Eq)
