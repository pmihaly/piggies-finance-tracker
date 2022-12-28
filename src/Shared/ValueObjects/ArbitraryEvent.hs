{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Shared.ValueObjects.ArbitraryEvent (ArbitraryEvent (..), arbitraryEventFromState, arbitraryAddedToPiggy, arbitraryTakenFromPiggy) where

import Application.Shared.State (State, piggyBalances)
import Data.HashMap.Strict qualified as Map
import Lens.Micro
import PiggyBalance.Entities.Piggy (balance, piggyId)
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
  eToPiggy <- (^. piggyId) <$> elements (Map.elems $ s ^. piggyBalances)
  AddedToPiggy eId eToPiggy <$> arbitrary

arbitraryTakenFromPiggy :: State -> Gen Event
arbitraryTakenFromPiggy s = do
  fromPiggy <- elements (Map.elems $ s ^. piggyBalances)

  eId <- arbitrary
  let eFromPiggyId = fromPiggy ^. piggyId
  eAmount <- arbitraryMaybeNotAvailableMoney $ fromPiggy ^. balance

  pure $ TakenFromPiggy eId eFromPiggyId eAmount

arbitraryMovedBetweenPiggies :: State -> Gen Event
arbitraryMovedBetweenPiggies s = do
  fromPiggy <- elements (Map.elems $ s ^. piggyBalances)
  toPiggy <- elements (Map.elems $ s ^. piggyBalances)

  eId <- arbitrary
  let eFromPiggyId = fromPiggy ^. piggyId
  let eToPiggyId = toPiggy ^. piggyId
  eAmount <- arbitraryMaybeNotAvailableMoney $ fromPiggy ^. balance

  pure $ MovedBetweenPiggies eId eFromPiggyId eToPiggyId eAmount

arbitraryAssetBought :: State -> Gen Event
arbitraryAssetBought s = do
  eId <- arbitrary
  ePiggy <- (^. piggyId) <$> elements (Map.elems $ s ^. piggyBalances)
  eAssetId <- arbitrary
  AssetBought eId ePiggy eAssetId <$> arbitrary

arbitraryAssetSold :: State -> Gen Event
arbitraryAssetSold s = do
  eId <- arbitrary
  ePiggy <- (^. piggyId) <$> elements (Map.elems $ s ^. piggyBalances)
  eAssetId <- arbitrary
  AssetSold eId ePiggy eAssetId <$> arbitrary

arbitraryAssetValueChanged :: Gen Event
arbitraryAssetValueChanged = do
  eId <- arbitrary
  eAssetId <- arbitrary
  AssetValueChanged eId eAssetId <$> arbitrary
