{-# LANGUAGE ImportQualifiedPost #-}

module PiggyBalance.OnEvent (onEvent) where

import Control.Arrow (left)
import Data.HashMap.Strict qualified as Map
import PiggyBalance.Entities.Piggy (Piggy, deposit, withdraw)
import PiggyBalance.PiggyBalances (PiggyBalances)
import PiggyBalance.ValueObjects.PiggyBalanceError (PiggyBalanceError (..))
import Shared.Entities.Event.Event (Event (..))
import Shared.ValueObjects.Id (Id)

onEvent :: PiggyBalances -> Event -> Either PiggyBalanceError PiggyBalances
onEvent balances (AddedToPiggy _ toPiggy amount) = do
  _ <- findPiggy toPiggy balances
  pure $ Map.adjust (deposit amount) toPiggy balances
onEvent balances (TakenFromPiggy _ fromPiggy maybeNotAvailableAmount) = do
  piggy <- findPiggy fromPiggy balances
  newPiggy <- left PiggyHasNotEnoughMoney $ withdraw maybeNotAvailableAmount piggy
  pure $ Map.adjust (const newPiggy) fromPiggy balances
onEvent balances _ = pure balances

findPiggy :: Id Piggy -> PiggyBalances -> Either PiggyBalanceError Piggy
findPiggy piggyId balances = maybe (Left $ PiggyNotFound piggyId) pure $ Map.lookup piggyId balances
