module PiggyBalance.OnEvent (onEvent) where

import PiggyBalance.PiggyBalances (PiggyBalances)
import PiggyBalance.ValueObjects.PiggyBalanceError (PiggyBalanceError)
import Shared.Entities.Event.Event (Event (..))

onEvent :: PiggyBalances -> Event -> Either PiggyBalanceError PiggyBalances
onEvent s (AddedToPiggy {}) = pure s
onEvent s _ = pure s
