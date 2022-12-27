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
        let newState = state & appliedEvents %~ Set.insert (getEventId event)
        newPiggyBalances <- left PiggyBalanceError $ PiggyBalance.onEvent (newState ^. piggyBalances) event
        ifNotApplied state event $ pure $ newState & piggyBalances .~ newPiggyBalances
    )

ifNotApplied :: State -> Event -> Either ApplicationError State -> Either ApplicationError State
ifNotApplied state event newState =
  if Set.member (getEventId event) (state ^. appliedEvents)
    then pure state
    else newState
