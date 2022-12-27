{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

module Shared.ValueObjects.Positive (Positive, unsafePositive, mkPositive, unPositive, PositiveError (..)) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON, withScientific)
import Data.Scientific (toBoundedInteger, toRealFloat)
import Test.QuickCheck (Arbitrary (..), suchThat)

newtype Positive a = Positive {unPositive :: a}
  deriving stock (Functor)
  deriving newtype (Show, Eq, Num, Fractional, ToJSON)
  deriving (RealFloat, RealFrac, Real, Floating, Ord) via a

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

instance (Arbitrary a, Num a, Eq a) => Arbitrary (Positive a) where
  arbitrary = do
    num <- arbitrary `suchThat` (\x -> signum x /= -1)
    return $ unsafePositive num

unsafePositive :: a -> Positive a
unsafePositive = Positive

data PositiveError
  = IllegalNegative
  | IllegalFloat
  deriving (Show, Eq)

mkPositive :: (Num a, Eq a) => a -> Either PositiveError (Positive a)
mkPositive x
  | signum x == 1 = Right $ unsafePositive x
  | signum x == 0 = Right $ unsafePositive x
  | otherwise = Left IllegalNegative
