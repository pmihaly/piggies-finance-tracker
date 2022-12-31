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
  newPiggy <- left PiggyHasNotEnoughMoney $ fst <$> withdraw maybeNotAvailableAmount piggy
  pure $ Map.adjust (const newPiggy) fromPiggy balances
onEvent balances (MovedBetweenPiggies _ fromPiggy toPiggy maybeNotAvailableAmount) = do
  sender <- findPiggy fromPiggy balances
  receiver <- findPiggy toPiggy balances
  (newSender, availableAmount) <- left PiggyHasNotEnoughMoney $ withdraw maybeNotAvailableAmount sender
  let newReceiver = deposit availableAmount receiver
  pure $ Map.adjust (const newReceiver) toPiggy $ Map.adjust (const newSender) fromPiggy balances
onEvent balances _ = pure balances

findPiggy :: Id Piggy -> PiggyBalances -> Either PiggyBalanceError Piggy
findPiggy piggyId balances = maybe (Left $ PiggyNotFound piggyId) pure $ Map.lookup piggyId balances
