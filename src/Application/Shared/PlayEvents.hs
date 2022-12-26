{-# LANGUAGE ImportQualifiedPost #-}

module Application.Shared.PlayEvents (playEvents) where

import Application.Shared.ApplicationError (ApplicationError (..))
import Application.Shared.State (State (..))
import Control.Arrow (left)
import Control.Monad (foldM)
import PiggyBalance.OnEvent qualified as PiggyBalance
import Shared.Entities.Event.Event (Event)

playEvents :: State -> [Event] -> Either ApplicationError State
playEvents =
  foldM
    ( \state' event -> do
        piggyBalances' <- left PiggyBalanceError $ PiggyBalance.onEvent (piggyBalances state') event
        pure $ State piggyBalances' (appliedEvents state')
    )
