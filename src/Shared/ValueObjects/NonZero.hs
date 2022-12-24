{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

module Shared.ValueObjects.NonZero (NonZero, unNonZero, unsafeNonZero, mkNonZero, NonZeroError (..)) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON, withScientific)
import Data.Scientific (toBoundedInteger, toRealFloat)

newtype NonZero a = NonZero {unNonZero :: a}
  deriving stock (Functor)
  deriving newtype (Show, Eq, Num, Fractional, ToJSON)
  deriving (RealFloat, RealFrac, Real, Floating, Ord) via a

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

unsafeNonZero :: a -> NonZero a
unsafeNonZero = NonZero

data NonZeroError
  = IllegalZero
  | IllegalFloat
  deriving (Show, Eq)

mkNonZero :: (Num a, Eq a) => a -> Either NonZeroError (NonZero a)
mkNonZero x
  | signum x == 1 = Right $ unsafeNonZero x
  | signum x == -1 = Right $ unsafeNonZero x
  | otherwise = Left IllegalZero
