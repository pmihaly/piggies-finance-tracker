{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Shared.ValueObjects.PiggyId (PiggyId (..), mkPiggyId, PiggyIdError (..)) where

import Control.Category ((>>>))
import Data.Bifunctor (bimap)
import Data.Text qualified as T
import Shared.ValueObjects.Id (Id, IdError, mkId)
import Test.QuickCheck (Arbitrary)

newtype PiggyId = UnsafePiggyId {unPiggyId :: Id}
  deriving newtype (Eq, Arbitrary)

instance Show PiggyId where
  show = unPiggyId >>> show >>> ("piggy-id " ++)

newtype PiggyIdError
  = IllegalPiggyId IdError
  deriving (Show, Eq)

mkPiggyId :: T.Text -> Either PiggyIdError PiggyId
mkPiggyId = mkId >>> bimap IllegalPiggyId UnsafePiggyId
