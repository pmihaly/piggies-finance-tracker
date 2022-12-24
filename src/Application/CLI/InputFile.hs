{-# LANGUAGE ImportQualifiedPost #-}

module Application.CLI.InputFile (InputFile (..)) where

import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:))
import Data.HashMap.Strict qualified as Map
import GHC.Generics (Generic)
import PiggyBalance.Entities.Piggy (Piggy)
import Shared.ValueObjects.Id (Id)

newtype InputFile = InputFile
  {piggyBalances :: Map.HashMap (Id Piggy) Piggy}
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON InputFile where
  parseJSON = withObject "InputFile" $ \obj -> do
    balances <- obj .: "piggy-balances"
    pure $ InputFile balances
