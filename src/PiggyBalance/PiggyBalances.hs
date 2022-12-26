{-# LANGUAGE ImportQualifiedPost #-}

module PiggyBalance.PiggyBalances (PiggyBalances (..), keys, member, getOneId, isEmpty) where

import Data.Aeson (FromJSON)
import Data.HashMap.Strict qualified as Map
import PiggyBalance.Entities.Piggy (Piggy)
import Shared.ValueObjects.Id (Id)
import Test.QuickCheck (Arbitrary (arbitrary))

newtype PiggyBalances = PiggyBalances (Map.HashMap (Id Piggy) Piggy) deriving (Show, Eq, FromJSON)

instance Arbitrary PiggyBalances where
  arbitrary = PiggyBalances . Map.fromList <$> arbitrary

keys :: PiggyBalances -> [Id Piggy]
keys (PiggyBalances bs) = Map.keys bs

member :: Id Piggy -> PiggyBalances -> Bool
member i (PiggyBalances bs) = Map.member i bs

getOneId :: PiggyBalances -> Id Piggy
getOneId (PiggyBalances bs) = fst $ head $ Map.toList bs

isEmpty :: PiggyBalances -> Bool
isEmpty (PiggyBalances bs) = Map.null bs
