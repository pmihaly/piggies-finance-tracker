{-# LANGUAGE EmptyDataDeriving #-}

module PiggyBalance.ValueObjects.PiggyBalanceError (PiggyBalanceError(..)) where

import PiggyBalance.Entities.Piggy (Piggy, PiggyError)
import Shared.ValueObjects.Id (Id)

data PiggyBalanceError
  = PiggyNotFound (Id Piggy)
  | PiggyHasNotEnoughMoney PiggyError
  deriving (Show, Eq)
