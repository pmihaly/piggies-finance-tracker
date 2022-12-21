{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Shared.ValueObjects.Positive (Positive (..), mkPositive, PositiveError (..)) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON, withScientific)
import Data.Scientific (toBoundedInteger, toRealFloat)

newtype Positive a = UnsafePositive {unPositive :: a}
  deriving stock (Functor)
  deriving newtype (Show, Eq, Num, Fractional, ToJSON)

instance {-# OVERLAPPING #-} FromJSON (Positive Int) where
  parseJSON =
    withScientific "Positive" $
      toBoundedInteger
        >>> maybe (Left IllegalFloat) Right
        >>> (>>= mkPositive)
        >>> either (show >>> fail) pure

instance (RealFloat a) => FromJSON (Positive a) where
  parseJSON =
    withScientific "Positive" $
      toRealFloat
        >>> mkPositive
        >>> either (show >>> fail) pure

data PositiveError
  = IllegalNegative
  | IllegalFloat
  deriving (Show, Eq)

mkPositive :: (Num a, Eq a) => a -> Either PositiveError (Positive a)
mkPositive x
  | signum x == 1 = Right $ UnsafePositive x
  | signum x == 0 = Right $ UnsafePositive x
  | otherwise = Left IllegalNegative
