{-# LANGUAGE ImportQualifiedPost #-}

module Application.CLI.InputFile (InputFile (..)) where

import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import GHC.Generics (Generic)
import PiggyBalance.Entities.Piggy (Piggy)
import Shared.Entities.Event.Event (Event)
import Shared.ValueObjects.Id (Id)

data InputFile = InputFile
  { piggyBalances :: Map.HashMap (Id Piggy) Piggy,
    events :: [Event],
    appliedEvents :: Set.HashSet (Id Event)
  }
  deriving (Show, Eq, Generic)

instance FromJSON InputFile where
  parseJSON = withObject "InputFile" $ \obj -> do
    balances <- obj .: "piggy-balances"
    events <- obj .:? "events" .!= []
    appliedEvents <- obj .:? "applied-events" .!= Set.empty
    pure $ InputFile balances events appliedEvents
