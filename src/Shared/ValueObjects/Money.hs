{-# LANGUAGE EmptyDataDeriving #-}

module Shared.ValueObjects.Money (Money, unsafeMoney, unMoney, MoneyError, mkMoney) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON, withScientific)
import Data.Scientific (toRealFloat)
import Test.QuickCheck (Arbitrary)

newtype Money = Money {unMoney :: Double}
  deriving newtype (Eq, Num, Fractional, RealFloat, RealFrac, Real, Floating, Ord, Arbitrary, ToJSON)

instance Show Money where
  show = unMoney >>> show

instance FromJSON Money where
  parseJSON =
    withScientific "Money" $
      toRealFloat
        >>> mkMoney
        >>> either (show >>> fail) pure

unsafeMoney :: Double -> Money
unsafeMoney = Money

data MoneyError deriving (Eq, Show)

mkMoney :: Double -> Either MoneyError Money
mkMoney = unsafeMoney >>> pure
