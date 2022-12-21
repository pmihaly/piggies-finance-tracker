{-# LANGUAGE ImportQualifiedPost #-}

module Shared.ValueObjects.EventId (EventId (..), mkEventId, EventIdError (..)) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.Bifunctor (bimap)
import Data.Text qualified as T
import Shared.ValueObjects.Id (Id, IdError, mkId)
import Test.QuickCheck (Arbitrary)

newtype EventId = UnsafeEventId {unEventId :: Id}
  deriving newtype (Eq, Arbitrary, ToJSON)

instance Show EventId where
  show = unEventId >>> show >>> ("event-id " ++)

instance FromJSON EventId where
  parseJSON = withText "EventId" $ mkEventId >>> either (show >>> fail) pure

newtype EventIdError
  = IllegalEventId IdError
  deriving (Show, Eq)

mkEventId :: T.Text -> Either EventIdError EventId
mkEventId = mkId >>> bimap IllegalEventId UnsafeEventId
