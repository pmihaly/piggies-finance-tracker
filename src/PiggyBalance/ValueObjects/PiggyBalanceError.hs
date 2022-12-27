{-# LANGUAGE EmptyDataDeriving #-}

module PiggyBalance.ValueObjects.PiggyBalanceError (PiggyBalanceError(..)) where

import PiggyBalance.Entities.Piggy (Piggy)
import Shared.ValueObjects.Id (Id)

newtype PiggyBalanceError
  = PiggyNotFound (Id Piggy)
  deriving (Show, Eq)
