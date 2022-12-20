{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shared.ValueObjects.NonZero (NonZero (..), mkNonZero, NonZeroError (..)) where

newtype NonZero a = UnsafeNonZero {unNonZero :: a}
  deriving stock (Functor)
  deriving newtype (Show, Eq, Num, Fractional)

data NonZeroError
  = IllegalZero
  deriving (Show, Eq)

mkNonZero :: (Num a, Eq a) => a -> Either NonZeroError (NonZero a)
mkNonZero x
  | signum x == 1 = Right $ UnsafeNonZero x
  | signum x == -1 = Right $ UnsafeNonZero x
  | otherwise = Left IllegalZero
