{-# LANGUAGE ImportQualifiedPost #-}

module Application.Shared.PlayEvents (playEvents) where

import Application.Shared.ApplicationError (ApplicationError (..))
import Application.Shared.State (State (..), appliedEvents, piggyBalances)
import Control.Arrow (left)
import Control.Monad (foldM)
import Data.HashSet qualified as Set
import Lens.Micro
import PiggyBalance.OnEvent qualified as PiggyBalance
import Shared.Entities.Event.Event (Event, getEventId)

playEvents :: State -> [Event] -> Either ApplicationError State
playEvents =
  foldM
    ( \state event -> do
        let state' = state & appliedEvents %~ Set.insert (getEventId event)
        piggyBalances' <- left PiggyBalanceError $ PiggyBalance.onEvent (state' ^. piggyBalances) event
        pure $ state' & piggyBalances .~ piggyBalances'
    )
