{-# LANGUAGE ImportQualifiedPost #-}

module Shared.ValueObjects.Text500 (Text500 (..), Text500Error (..), mkText500) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.Text qualified as T
import Test.QuickCheck (Arbitrary (arbitrary))

newtype Text500 = UnsafeText500 {unText500 :: T.Text}
  deriving newtype (Eq, ToJSON)

instance FromJSON Text500 where
  parseJSON =
    withText "Text500" $
      mkText500
        >>> either (show >>> fail) pure

instance Show Text500 where
  show = unText500 >>> show

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
