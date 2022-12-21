{-# LANGUAGE ImportQualifiedPost #-}

module Shared.ValueObjects.Id (Id (..), mkId, IdError (..)) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.Bifunctor (bimap)
import Data.String (IsString)
import Data.Text qualified as T
import Shared.ValueObjects.Text50 (Text50, Text50Error, mkText50)
import Test.QuickCheck (Arbitrary)

newtype Id = UnsafeId {unId :: Text50}
  deriving newtype (Show, Eq, Arbitrary, ToJSON, IsString)

instance FromJSON Id where
  parseJSON = withText "Id" $ mkId >>> either (show >>> fail) pure

newtype IdError
  = IllegalId Text50Error
  deriving (Show, Eq)

mkId :: T.Text -> Either IdError Id
mkId = mkText50 >>> bimap IllegalId UnsafeId
