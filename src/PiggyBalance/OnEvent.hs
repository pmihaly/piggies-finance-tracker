{-# LANGUAGE ImportQualifiedPost #-}

module PiggyBalance.OnEvent (onEvent) where

import Data.HashMap.Strict qualified as Map
import PiggyBalance.Entities.Piggy (deposit)
import PiggyBalance.PiggyBalances (PiggyBalances)
import PiggyBalance.ValueObjects.PiggyBalanceError (PiggyBalanceError (..))
import Shared.Entities.Event.Event (Event (..))

onEvent :: PiggyBalances -> Event -> Either PiggyBalanceError PiggyBalances
onEvent balances (AddedToPiggy _ toPiggy amount) =
  if Map.member toPiggy balances
    then pure $ Map.adjust (deposit amount) toPiggy balances
    else Left $ PiggyNotFound toPiggy
onEvent balances _ = pure balances
