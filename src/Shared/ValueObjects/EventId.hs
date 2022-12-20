{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Shared.ValueObjects.EventId (EventId (..), mkEventId, EventIdError (..)) where

import Control.Category ((>>>))
import Data.Bifunctor (bimap)
import Data.Text qualified as T
import Shared.ValueObjects.Id (Id, IdError, mkId)
import Test.QuickCheck (Arbitrary)

newtype EventId = UnsafeEventId {unEventId :: Id}
  deriving newtype (Eq, Arbitrary)

instance Show EventId where
  show = unEventId >>> show >>> ("event-id " ++)

newtype EventIdError
  = IllegalEventId IdError
  deriving (Show, Eq)

mkEventId :: T.Text -> Either EventIdError EventId
mkEventId = mkId >>> bimap IllegalEventId UnsafeEventId
