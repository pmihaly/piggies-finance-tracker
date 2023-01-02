{-# LANGUAGE ImportQualifiedPost #-}

module PiggyBalance.PiggyBalances (PiggyBalances) where

import Data.HashMap.Strict qualified as Map
import PiggyBalance.Entities.Piggy (Piggy)
import Shared.ValueObjects.Id (Id)

type PiggyBalances = Map.HashMap (Id Piggy) Piggy
