{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Shared.ValueObjects.Text500 (Text500 (..), Text500Error (..), mkText500) where
import Test.QuickCheck (Arbitrary (arbitrary))

import Data.Text qualified as T

newtype Text500 = UnsafeText500 {unText500 :: T.Text}
  deriving newtype (Show, Eq)

instance Arbitrary Text500 where
  arbitrary = UnsafeText500 . T.pack <$> arbitrary

data Text500Error
  = TooShort
  | TooLong
  deriving (Eq, Show)

mkText500 :: T.Text -> Either Text500Error Text500
mkText500 x
  | x == "" = Left TooShort
  | T.length x > 500 = Left TooLong
  | otherwise = Right $ UnsafeText500 x
