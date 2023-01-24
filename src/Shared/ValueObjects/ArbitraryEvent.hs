{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Shared.ValueObjects.ArbitraryEvent (ArbitraryEvent (..), arbitraryEventFromState, arbitraryAddedToPiggy, arbitraryTakenFromPiggy) where

import Application.Shared.State (State, piggyBalances)
import Data.HashMap.Strict qualified as Map
import Lens.Micro
import PiggyBalance.Entities.Piggy (balance)
import Shared.Entities.Event.Event (Event (..))
import Shared.ValueObjects.MaybeNotAvailable (arbitraryMaybeNotAvailableMoney)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements)
import Test.QuickCheck.Gen (oneof)

newtype ArbitraryEvent = ArbitraryEvent {unArbitraryEvent :: Event} deriving (Show, Eq)

instance Arbitrary ArbitraryEvent where
  arbitrary = do
    state <- arbitrary
    ArbitraryEvent <$> arbitraryEventFromState state

arbitraryEventFromState :: State -> Gen Event
arbitraryEventFromState s =
  do
    oneof
      [ arbitraryAddedToPiggy s,
        arbitraryTakenFromPiggy s,
        arbitraryMovedBetweenPiggies s,
        arbitraryAssetBought s,
        arbitraryAssetSold s,
        arbitraryAssetValueChanged
      ]

arbitraryAddedToPiggy :: State -> Gen Event
arbitraryAddedToPiggy s = do
  eId <- arbitrary
  eToPiggy <- elements (Map.keys $ s ^. piggyBalances)
  AddedToPiggy eId eToPiggy <$> arbitrary

arbitraryTakenFromPiggy :: State -> Gen Event
arbitraryTakenFromPiggy s = do
  (eFromPiggyId, fromPiggy) <- elements (Map.toList $ s ^. piggyBalances)

  eId <- arbitrary
  eAmount <- arbitraryMaybeNotAvailableMoney $ fromPiggy ^. balance

  pure $ TakenFromPiggy eId eFromPiggyId eAmount

arbitraryMovedBetweenPiggies :: State -> Gen Event
arbitraryMovedBetweenPiggies s = do
  (eFromPiggyId, fromPiggy) <- elements (Map.toList $ s ^. piggyBalances)
  eToPiggyId <- elements (Map.keys $ s ^. piggyBalances)

  eId <- arbitrary
  eAmount <- arbitraryMaybeNotAvailableMoney $ fromPiggy ^. balance

  pure $ MovedBetweenPiggies eId eFromPiggyId eToPiggyId eAmount

arbitraryAssetBought :: State -> Gen Event
arbitraryAssetBought s = do
  eId <- arbitrary
  ePiggy <- elements (Map.keys $ s ^. piggyBalances)
  eAssetId <- arbitrary
  AssetBought eId ePiggy eAssetId <$> arbitrary

arbitraryAssetSold :: State -> Gen Event
arbitraryAssetSold s = do
  eId <- arbitrary
  ePiggy <- elements (Map.keys $ s ^. piggyBalances)
  eAssetId <- arbitrary
  AssetSold eId ePiggy eAssetId <$> arbitrary

arbitraryAssetValueChanged :: Gen Event
arbitraryAssetValueChanged = do
  eId <- arbitrary
  eAssetId <- arbitrary
  AssetValueChanged eId eAssetId <$> arbitrary
