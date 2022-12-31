{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module PiggyBalance.Entities.Piggy (Piggy, unsafePiggy, piggyId, balance, PiggyError (..), mkPiggy, deposit, withdraw) where

import Control.Arrow (left)
import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Platform (makeLenses)
import PiggyBalance.ValueObjects.Balance (Balance, addMoney, subtractMoney)
import Shared.ValueObjects.Id (Id)
import Shared.ValueObjects.MaybeNotAvailable (MaybeNotAvailable, MaybeNotAvailableError, unmkMaybeNotAvailableMoney)
import Shared.ValueObjects.Money (Money)
import Shared.ValueObjects.NonZero (NonZero)
import Shared.ValueObjects.Positive (Positive)
import Test.QuickCheck (Arbitrary (arbitrary))

data Piggy = Piggy
  { _piggyId :: Id Piggy,
    _balance :: Balance
  }
  deriving (Eq, Show, Generic)

makeLenses ''Piggy

instance Arbitrary Piggy where
  arbitrary = do
    aPiggyId <- arbitrary
    Piggy aPiggyId <$> arbitrary

instance ToJSON Piggy where
  toJSON (Piggy pId pBalance) = object ["id" .= pId, "balance" .= pBalance]

instance FromJSON Piggy where
  parseJSON = withObject "Piggy" $ \obj -> do
    pId <- obj .: "id"
    pBalance <- obj .: "balance"
    either (show >>> fail) pure $ mkPiggy pId pBalance

unsafePiggy :: Id Piggy -> Balance -> Piggy
unsafePiggy = Piggy

newtype PiggyError
  = InsufficientFunds MaybeNotAvailableError
  deriving (Eq, Show)

mkPiggy :: Id Piggy -> Balance -> Either PiggyError Piggy
mkPiggy pId pBalance = pure $ unsafePiggy pId pBalance

deposit :: NonZero (Positive Money) -> Piggy -> Piggy
deposit amount piggy = piggy & balance %~ addMoney amount

withdraw :: MaybeNotAvailable (NonZero (Positive Money)) -> Piggy -> Either PiggyError (Piggy, NonZero (Positive Money))
withdraw mnaAmount piggy = do
  amount <- left InsufficientFunds $ unmkMaybeNotAvailableMoney mnaAmount $ piggy ^. balance
  pure (piggy & balance %~ subtractMoney amount, amount)
