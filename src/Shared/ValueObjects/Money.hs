{-# LANGUAGE EmptyDataDeriving #-}

module Shared.ValueObjects.Money (Money (..), MoneyError, mkMoney) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON, withScientific)
import Data.Scientific (toRealFloat)
import Test.QuickCheck (Arbitrary)

newtype Money = UnsafeMoney {unMoney :: Double}
  deriving newtype (Eq, Num, Fractional, Arbitrary, ToJSON)

instance Show Money where
  show = unMoney >>> show

instance FromJSON Money where
  parseJSON =
    withScientific "Money" $
      toRealFloat
        >>> mkMoney
        >>> either (show >>> fail) pure

data MoneyError deriving (Eq, Show)

mkMoney :: Double -> Either MoneyError Money
mkMoney = UnsafeMoney >>> pure
