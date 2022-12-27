{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}

module PiggyBalance.Entities.Piggy (Piggy, unsafePiggy, balance, PiggyError, mkPiggy, deposit) where

import Control.Arrow ((&&&), (***))
import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:))
import GHC.Generics (Generic)
import PiggyBalance.ValueObjects.Balance (Balance, addMoney)
import Shared.ValueObjects.Id (Id)
import Shared.ValueObjects.Money (Money)
import Shared.ValueObjects.NonZero (NonZero)
import Shared.ValueObjects.Positive (Positive)
import Test.QuickCheck (Arbitrary (arbitrary))

data Piggy where
  Piggy :: {balanceId :: Id Piggy, balance :: Balance} -> Piggy
  deriving (Eq, Generic)

instance Arbitrary Piggy where
  arbitrary = do
    balanceId <- arbitrary
    balance <- arbitrary
    return Piggy {balanceId = balanceId, balance = balance}

instance Show Piggy where
  show =
    (balanceId &&& balance)
      >>> (show *** show)
      >>> (\(i, b) -> "piggy with id of " <> i <> " and balance of " <> b)

instance ToJSON Piggy

instance FromJSON Piggy where
  parseJSON = withObject "Piggy" $ \obj -> do
    pId <- obj .: "id"
    pBalance <- obj .: "balance"
    either (show >>> fail) pure $ mkPiggy pId pBalance

unsafePiggy :: Id Piggy -> Balance -> Piggy
unsafePiggy = Piggy

data PiggyError deriving (Eq, Show)

mkPiggy :: Id Piggy -> Balance -> Either PiggyError Piggy
mkPiggy pBalanceId pBalance = pure $ unsafePiggy pBalanceId pBalance

deposit :: NonZero (Positive Money) -> Piggy -> Piggy
deposit amount piggy = unsafePiggy (balanceId piggy) (addMoney amount (balance piggy))
