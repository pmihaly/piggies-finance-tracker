{-# LANGUAGE ImportQualifiedPost #-}

module Application.CLI.InputFile (InputFile (..)) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.HashMap.Strict qualified as Map
import GHC.Generics (Generic)
import PiggyBalance.Entities.Piggy (Piggy)
import Shared.Entities.Event.Event (Event)
import Shared.ValueObjects.Id (Id)

data InputFile = InputFile
  { piggyBalances :: Map.HashMap (Id Piggy) Piggy,
    events :: [Event] }
  deriving (Show, Eq, Generic)

instance FromJSON InputFile where
  parseJSON = withObject "InputFile" $ \obj -> do
    balances <- obj .: "piggy-balances"
    events <- obj .: "events"
    pure $ InputFile balances events
