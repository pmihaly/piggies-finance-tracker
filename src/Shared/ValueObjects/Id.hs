{-# LANGUAGE ImportQualifiedPost #-}

module Shared.ValueObjects.Id (Id, unId, mkId, IdError (..), unsafeId) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSONKey, FromJSONKey, ToJSON, withText)
import Data.Bifunctor (bimap)
import Data.Text qualified as T
import Shared.ValueObjects.Text50 (Text50, Text50Error, mkText50)
import Test.QuickCheck (Arbitrary)
import Data.Hashable (Hashable)

newtype Id a = Id {unId :: Text50}
  deriving newtype (Show, Eq, Arbitrary, ToJSON, Hashable)

instance FromJSON (Id a) where
  parseJSON = withText "Id" $ mkId >>> either (show >>> fail) pure

instance FromJSONKey (Id a)
instance ToJSONKey (Id a)

unsafeId :: Text50 -> Id a
unsafeId = Id

newtype IdError
  = IllegalId Text50Error
  deriving (Show, Eq)

mkId :: T.Text -> Either IdError (Id a)
mkId = mkText50 >>> bimap IllegalId unsafeId
