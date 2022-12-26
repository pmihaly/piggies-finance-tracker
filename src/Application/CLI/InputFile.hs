{-# LANGUAGE ImportQualifiedPost #-}

module Application.CLI.InputFile (InputFile (..), toState) where

import Application.Shared.State qualified as State
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.HashSet qualified as Set
import GHC.Generics (Generic)
import PiggyBalance.PiggyBalances (PiggyBalances)
import Shared.Entities.Event.Event (Event)
import Shared.ValueObjects.Id (Id)

data InputFile = InputFile
  { piggyBalances :: PiggyBalances,
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

toState :: InputFile -> State.State
toState i = State.State (piggyBalances i) (appliedEvents i)
