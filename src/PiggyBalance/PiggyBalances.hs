{-# LANGUAGE ImportQualifiedPost #-}

module PiggyBalance.PiggyBalances (PiggyBalances (..)) where

import Data.HashMap.Strict qualified as Map
import PiggyBalance.Entities.Piggy (Piggy)
import Shared.ValueObjects.Id (Id)
import Test.QuickCheck (Arbitrary (arbitrary))
import Data.Aeson (FromJSON)

newtype PiggyBalances = PiggyBalances (Map.HashMap (Id Piggy) Piggy) deriving (Show, Eq, FromJSON)

instance Arbitrary PiggyBalances where
  arbitrary = PiggyBalances . Map.fromList <$> arbitrary
