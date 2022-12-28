{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}

module Shared.ValueObjects.Available (Available, unAvailable, mkAvailableMoney, AvailableError (..), unsafeAvailable) where

import Data.Aeson (FromJSON, ToJSON)
import PiggyBalance.ValueObjects.Balance (Balance, unBalance)
import Shared.ValueObjects.Money (Money)
import Shared.ValueObjects.NonZero (NonZero, unNonZero)
import Shared.ValueObjects.Positive (Positive, unPositive)

newtype Available a = Available {unAvailable :: a}
  deriving stock (Functor)
  deriving newtype (Show, Eq, Num, Fractional, FromJSON, ToJSON)
  deriving (RealFloat, RealFrac, Real, Floating, Ord) via a

unsafeAvailable :: a -> Available a
unsafeAvailable = Available

newtype AvailableError
  = NotEnoughMoney Money
  deriving (Show, Eq)

mkAvailableMoney :: NonZero (Positive Money) -> Balance -> Either AvailableError (Available (NonZero (Positive Money)))
mkAvailableMoney m b
  | unPositive (unNonZero m) > unBalance b = Left $ NotEnoughMoney $ unPositive $ unNonZero m
  | otherwise = Right $ unsafeAvailable m
