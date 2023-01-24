{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module PiggyBalance.Entities.Piggy (Piggy, unsafePiggy, balance, PiggyError (..), mkPiggy, deposit, withdraw) where

import Control.Arrow (left)
import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Platform (makeLenses)
import PiggyBalance.ValueObjects.Balance (Balance, addMoney, subtractMoney)
import Shared.ValueObjects.MaybeNotAvailable (MaybeNotAvailable, MaybeNotAvailableError, unmkMaybeNotAvailableMoney)
import Shared.ValueObjects.Money (Money)
import Shared.ValueObjects.NonZero (NonZero)
import Shared.ValueObjects.Positive (Positive)
import Test.QuickCheck (Arbitrary (arbitrary))

newtype Piggy = Piggy {_balance :: Balance}
  deriving (Eq, Show, Generic)

makeLenses ''Piggy

instance Arbitrary Piggy where
  arbitrary = do
    Piggy <$> arbitrary

instance ToJSON Piggy where
  toJSON (Piggy pBalance) = object ["balance" .= pBalance]

instance FromJSON Piggy where
  parseJSON = withObject "Piggy" $ \obj -> do
    pBalance <- obj .: "balance"
    either (show >>> fail) pure $ mkPiggy pBalance

unsafePiggy :: Balance -> Piggy
unsafePiggy = Piggy

newtype PiggyError
  = InsufficientFunds MaybeNotAvailableError
  deriving (Eq, Show)

mkPiggy :: Balance -> Either PiggyError Piggy
mkPiggy = unsafePiggy >>> pure

deposit :: NonZero (Positive Money) -> Piggy -> Piggy
deposit amount piggy = piggy & balance %~ addMoney amount

withdraw :: MaybeNotAvailable (NonZero (Positive Money)) -> Piggy -> Either PiggyError (Piggy, NonZero (Positive Money))
withdraw mnaAmount piggy = do
  amount <- left InsufficientFunds $ unmkMaybeNotAvailableMoney mnaAmount $ piggy ^. balance
  pure (piggy & balance %~ subtractMoney amount, amount)
