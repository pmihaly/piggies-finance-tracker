{-# LANGUAGE ImportQualifiedPost #-}

module Application.Shared.PlayEvents (playEvents) where

import Application.Shared.ApplicationError (ApplicationError (..))
import Application.Shared.State (State (..))
import Control.Arrow (left)
import Control.Monad (foldM)
import Data.HashSet qualified as Set
import PiggyBalance.OnEvent qualified as PiggyBalance
import Shared.Entities.Event.Event (Event, getEventId)

playEvents :: State -> [Event] -> Either ApplicationError State
playEvents =
  foldM
    ( \state event -> do
        let state' = State (piggyBalances state) (Set.insert (getEventId event) $ appliedEvents state)
        piggyBalances' <- left PiggyBalanceError $ PiggyBalance.onEvent (piggyBalances state') event
        pure $ State piggyBalances' (appliedEvents state')
    )
