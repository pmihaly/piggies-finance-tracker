{-# LANGUAGE ImportQualifiedPost #-}

module Shared.ValueObjects.Text50 (Text50 (..), Text50Error (..), mkText50) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.Text qualified as T
import Test.QuickCheck (Arbitrary (arbitrary))

newtype Text50 = UnsafeText50 {unText50 :: T.Text}
  deriving newtype (Eq, ToJSON)

instance FromJSON Text50 where
  parseJSON =
    withText "Text50" $
      mkText50
        >>> either (show >>> fail) pure

instance Show Text50 where
  show = unText50 >>> show

instance Arbitrary Text50 where
  arbitrary = UnsafeText50 . T.pack <$> arbitrary

data Text50Error
  = TooShort
  | TooLong
  deriving (Eq, Show)

mkText50 :: T.Text -> Either Text50Error Text50
mkText50 x
  | x == "" = Left TooShort
  | T.length x > 50 = Left TooLong
  | otherwise = Right $ UnsafeText50 x
