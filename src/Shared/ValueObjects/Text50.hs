{-# LANGUAGE ImportQualifiedPost #-}

module Shared.ValueObjects.Text50 (Text50, unsafeText50, Text50Error (..), mkText50, unText50) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.Hashable (Hashable)
import Data.Text qualified as T
import Test.QuickCheck (Arbitrary (arbitrary), suchThat)

newtype Text50 = Text50 {unText50 :: T.Text}
  deriving newtype (Eq, ToJSON, Hashable)

instance FromJSON Text50 where
  parseJSON =
    withText "Text50" $
      mkText50
        >>> either (show >>> fail) pure

instance Show Text50 where
  show = unText50 >>> show

instance Arbitrary Text50 where
  arbitrary = do
    str <- T.pack <$> arbitrary `suchThat` (not . null)
    return $ unsafeText50 $ T.take 50 str

unsafeText50 :: T.Text -> Text50
unsafeText50 = Text50

data Text50Error
  = TooShort
  | TooLong
  deriving (Eq, Show)

mkText50 :: T.Text -> Either Text50Error Text50
mkText50 x
  | x == "" = Left TooShort
  | T.length x > 50 = Left TooLong
  | otherwise = Right $ unsafeText50 x
