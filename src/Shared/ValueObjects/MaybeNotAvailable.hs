{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}

module Shared.ValueObjects.MaybeNotAvailable (MaybeNotAvailable (..), unmkMaybeNotAvailableMoney, MaybeNotAvailableError (..), arbitraryMaybeNotAvailableMoney) where

import Data.Aeson (FromJSON, ToJSON)
import PiggyBalance.ValueObjects.Balance (Balance, unBalance)
import Shared.ValueObjects.Money (Money)
import Shared.ValueObjects.NonZero (NonZero, unNonZero)
import Shared.ValueObjects.Positive (Positive, unPositive)
import Test.QuickCheck (Gen, arbitrary, suchThat)

newtype MaybeNotAvailable a = MaybeNotAvailable {unsafeUnMaybeNotAvailable :: a}
  deriving stock (Functor)
  deriving newtype (Show, Eq, Num, Fractional, FromJSON, ToJSON)
  deriving (RealFloat, RealFrac, Real, Floating, Ord) via a

newtype MaybeNotAvailableError
  = NotEnoughMoney Money
  deriving (Show, Eq)

unmkMaybeNotAvailableMoney :: MaybeNotAvailable (NonZero (Positive Money)) -> Balance -> Either MaybeNotAvailableError (NonZero (Positive Money))
unmkMaybeNotAvailableMoney (MaybeNotAvailable m) b
  | unPositive (unNonZero m) > unBalance b = Left $ NotEnoughMoney $ unPositive $ unNonZero m
  | otherwise = Right m

arbitraryMaybeNotAvailableMoney :: Balance -> Gen (MaybeNotAvailable (NonZero (Positive Money)))
arbitraryMaybeNotAvailableMoney b = MaybeNotAvailable <$> arbitrary `suchThat` (\m -> unPositive (unNonZero m) <= unBalance b)
