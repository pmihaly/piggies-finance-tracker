module PiggyBalance.ValueObjects.Balance (Balance, unsafeBalance, unBalance, BalanceError, mkBalance, addMoney) where

import Control.Arrow (left)
import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON, withScientific)
import Data.Scientific (toRealFloat)
import Shared.ValueObjects.Money (Money, MoneyError, mkMoney)
import Shared.ValueObjects.NonZero (NonZero, NonZeroError, mkNonZero, unNonZero)
import Shared.ValueObjects.Positive (Positive (unPositive))
import Test.QuickCheck (Arbitrary (..), suchThat)

newtype Balance = Balance {unBalance :: Money}
  deriving newtype (Eq, Num, Fractional, ToJSON)

instance Show Balance where
  show = unBalance >>> show

instance FromJSON Balance where
  parseJSON =
    withScientific "Balance" $
      toRealFloat
        >>> (mkMoney >>> left BalanceMoneyError)
        >>> (>>= mkNonZero >>> left CreatingABalanceWithZeroMoney)
        >>> (>>= mkBalance)
        >>> either (show >>> fail) pure

instance Arbitrary Balance where
  arbitrary = do
    m <- arbitrary `suchThat` (pure >>> (/= mkMoney 0.0))
    return $ unsafeBalance m

unsafeBalance :: Money -> Balance
unsafeBalance = Balance

data BalanceError
  = CreatingABalanceWithZeroMoney NonZeroError
  | BalanceMoneyError MoneyError
  deriving (Eq, Show)

mkBalance :: NonZero Money -> Either BalanceError Balance
mkBalance = unNonZero >>> unsafeBalance >>> pure

addMoney :: NonZero (Positive Money) -> Balance -> Balance
addMoney amount balance = unsafeBalance (unPositive (unNonZero amount) + unBalance balance)
