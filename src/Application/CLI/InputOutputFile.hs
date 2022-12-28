{-# LANGUAGE ImportQualifiedPost #-}

module Application.CLI.InputOutputFile (InputOutputFile (..), toState, fromStateWithEvents) where

import Application.Shared.State qualified as State
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.HashSet qualified as Set
import GHC.Generics (Generic)
import Lens.Micro
import PiggyBalance.PiggyBalances (PiggyBalances)
import Shared.ValueObjects.ArbitraryEvent (ArbitraryEvent (..))
import Shared.Entities.Event.Event (Event)
import Shared.ValueObjects.Id (Id)
import Test.QuickCheck (Arbitrary (..))

data InputOutputFile = InputOutputFile
  { piggyBalances :: PiggyBalances,
    events :: [Event],
    appliedEvents :: Set.HashSet (Id Event)
  }
  deriving (Show, Eq, Generic)

instance FromJSON InputOutputFile where
  parseJSON = withObject "InputOutputFile" $ \obj -> do
    balances <- obj .: "piggy-balances"
    events <- obj .:? "events" .!= []
    appliedEvents <- obj .:? "applied-events" .!= Set.empty
    pure $ InputOutputFile balances events appliedEvents

instance ToJSON InputOutputFile where
  toJSON (InputOutputFile iBalances iEvents iAppliedEvents) =
    object
      [ "piggy-balances" .= iBalances,
        "events" .= iEvents,
        "applied-events" .= iAppliedEvents
      ]

instance Arbitrary InputOutputFile where
  arbitrary = do
    events <- fmap unArbitraryEvent <$> arbitrary
    fromStateWithEvents events <$> arbitrary

toState :: InputOutputFile -> State.State
toState i = State.State (piggyBalances i) (appliedEvents i)

fromStateWithEvents :: [Event] -> State.State -> InputOutputFile
fromStateWithEvents es s = InputOutputFile (s ^. State.piggyBalances) es (s ^. State.appliedEvents)
