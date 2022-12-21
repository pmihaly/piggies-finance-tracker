{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

module Shared.ValueObjects.NonZero (NonZero (..), mkNonZero, NonZeroError (..)) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON, withScientific)
import Data.Scientific (toBoundedInteger, toRealFloat)

newtype NonZero a = UnsafeNonZero {unNonZero :: a}
  deriving stock (Functor)
  deriving newtype (Show, Eq, Num, Fractional, ToJSON)

instance {-# OVERLAPPING #-} FromJSON (NonZero Int) where
  parseJSON =
    withScientific "NonZero" $
      toBoundedInteger
        >>> maybe (Left IllegalFloat) Right
        >>> (>>= mkNonZero)
        >>> either (show >>> fail) pure

instance (RealFloat a) => FromJSON (NonZero a) where
  parseJSON =
    withScientific "NonZero" $
      toRealFloat
        >>> mkNonZero
        >>> either (show >>> fail) pure

data NonZeroError
  = IllegalZero
  | IllegalFloat
  deriving (Show, Eq)

mkNonZero :: (Num a, Eq a) => a -> Either NonZeroError (NonZero a)
mkNonZero x
  | signum x == 1 = Right $ UnsafeNonZero x
  | signum x == -1 = Right $ UnsafeNonZero x
  | otherwise = Left IllegalZero
