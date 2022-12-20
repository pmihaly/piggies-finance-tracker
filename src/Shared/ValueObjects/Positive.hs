{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shared.ValueObjects.Positive (Positive (..), mkPositive, PositiveError (..)) where

newtype Positive a = UnsafePositive {unPositive :: a}
  deriving stock (Functor)
  deriving newtype (Show, Eq, Num, Fractional)

data PositiveError
  = IllegalNegative
  deriving (Show, Eq)

mkPositive :: (Num a, Eq a) => a -> Either PositiveError (Positive a)
mkPositive x
  | signum x == 1 = Right $ UnsafePositive x
  | signum x == 0 = Right $ UnsafePositive x
  | otherwise = Left IllegalNegative
