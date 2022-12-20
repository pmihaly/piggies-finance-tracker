{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Shared.ValueObjects.Money (Money (..), MoneyError, mkMoney) where

import Control.Category ((>>>))
import Test.QuickCheck (Arbitrary)

newtype Money = UnsafeMoney {unMoney :: Double}
  deriving newtype (Eq, Num, Fractional, Arbitrary)

instance Show Money where
  show = unMoney >>> show

data MoneyError deriving (Eq, Show)

mkMoney :: Double -> Either MoneyError Money
mkMoney = UnsafeMoney >>> pure
