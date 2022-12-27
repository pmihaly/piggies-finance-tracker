{-# LANGUAGE ImportQualifiedPost #-}

module PiggyBalance.OnEvent (onEvent) where

import Data.HashMap.Strict qualified as Map
import PiggyBalance.Entities.Piggy (Piggy, deposit)
import PiggyBalance.PiggyBalances (PiggyBalances)
import PiggyBalance.ValueObjects.PiggyBalanceError (PiggyBalanceError (..))
import Shared.Entities.Event.Event (Event (..))
import Shared.ValueObjects.Id (Id)

onEvent :: PiggyBalances -> Event -> Either PiggyBalanceError PiggyBalances
onEvent balances (AddedToPiggy _ toPiggy amount) = ifExists toPiggy balances $ Map.adjust (deposit amount) toPiggy balances
onEvent balances _ = pure balances

ifExists :: Id Piggy -> PiggyBalances -> PiggyBalances -> Either PiggyBalanceError PiggyBalances
ifExists toPiggy balances newBalances =
  if Map.member toPiggy balances
    then pure newBalances
    else Left $ PiggyNotFound toPiggy
