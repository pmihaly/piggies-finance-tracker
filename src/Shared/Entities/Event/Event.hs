{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Shared.Entities.Event.Event (Event (..), getEventId, arbitraryEventWithPiggyIds) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Aeson.Key (fromString)
import GHC.Generics (Generic)
import PiggyBalance.Entities.Piggy (Piggy)
import Shared.ValueObjects.Id (Id)
import Shared.ValueObjects.Money (Money)
import Shared.ValueObjects.NonZero qualified as NonZero
import Shared.ValueObjects.Positive (Positive)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements)
import Test.QuickCheck.Gen (oneof)

data Event
  = AddedToPiggy EventId ToPiggy (NonZero.NonZero (Positive Money))
  | TakenFromPiggy EventId FromPiggy (NonZero.NonZero (Positive Money))
  | MovedBetweenPiggies EventId FromPiggy ToPiggy (NonZero.NonZero (Positive Money))
  | AssetBought EventId PiggyId AssetId Shares
  | AssetSold EventId PiggyId AssetId Shares
  | AssetValueChanged EventId AssetId Percentage
  deriving (Eq, Show, Generic)

instance FromJSON Event where
  parseJSON = withObject "Event" $ \obj -> do
    (eventType :: String) <- obj .: fromString "type"
    case eventType of
      "added-to-piggy" -> do
        eId <- obj .: "id"
        eToPiggy <- obj .: "to-piggy"
        eAmount <- obj .: "amount"
        pure $ AddedToPiggy eId eToPiggy eAmount
      "taken-from-piggy" -> do
        eId <- obj .: "id"
        eFromPiggy <- obj .: "from-piggy"
        eAmount <- obj .: "amount"
        pure $ TakenFromPiggy eId eFromPiggy eAmount
      "moved-between-piggies" -> do
        eId <- obj .: "id"
        eFromPiggy <- obj .: "from-piggy"
        eToPiggy <- obj .: "to-piggy"
        eAmount <- obj .: "amount"
        pure $ MovedBetweenPiggies eId eFromPiggy eToPiggy eAmount
      "asset-bought" -> do
        eId <- obj .: "id"
        ePiggyId <- obj .: "piggy"
        eAssetId <- obj .: "name"
        eShares <- obj .: "shares"
        pure $ AssetBought eId ePiggyId eAssetId eShares
      "asset-sold" -> do
        eId <- obj .: "id"
        ePiggyId <- obj .: "piggy"
        eAssetId <- obj .: "investment"
        eShares <- obj .: "shares"
        pure $ AssetSold eId ePiggyId eAssetId eShares
      "asset-value-changed" -> do
        eId <- obj .: "id"
        eAssetId <- obj .: "name"
        ePercentage <- obj .: "percentage"
        pure $ AssetValueChanged eId eAssetId ePercentage
      _ -> fail "Invalid event type"

instance Arbitrary Event where
  arbitrary = do
    piggyIds <- arbitrary
    arbitraryEventWithPiggyIds piggyIds

arbitraryEventWithPiggyIds :: [Id Piggy] -> Gen Event
arbitraryEventWithPiggyIds piggyIds =
  oneof
    [ arbitraryAddedToPiggy piggyIds,
      arbitraryTakenFromPiggy piggyIds,
      arbitraryMovedBetweenPiggies piggyIds,
      arbitraryAssetBought piggyIds,
      arbitraryAssetSold piggyIds,
      arbitraryAssetValueChanged
    ]

arbitraryAddedToPiggy :: [Id Piggy] -> Gen Event
arbitraryAddedToPiggy piggyIds = do
  eId <- arbitrary
  eToPiggy <- elements piggyIds
  AddedToPiggy eId eToPiggy <$> arbitrary

arbitraryTakenFromPiggy :: [Id Piggy] -> Gen Event
arbitraryTakenFromPiggy piggyIds = do
  eId <- arbitrary
  eFromPiggy <- elements piggyIds
  TakenFromPiggy eId eFromPiggy <$> arbitrary

arbitraryMovedBetweenPiggies :: [Id Piggy] -> Gen Event
arbitraryMovedBetweenPiggies piggyIds = do
  eId <- arbitrary
  eFromPiggy <- elements piggyIds
  eToPiggy <- elements piggyIds
  MovedBetweenPiggies eId eFromPiggy eToPiggy <$> arbitrary

arbitraryAssetBought :: [Id Piggy] -> Gen Event
arbitraryAssetBought piggyIds = do
  eId <- arbitrary
  ePiggyId <- elements piggyIds
  eAssetId <- arbitrary
  AssetBought eId ePiggyId eAssetId <$> arbitrary

arbitraryAssetSold :: [Id Piggy] -> Gen Event
arbitraryAssetSold piggyIds = do
  eId <- arbitrary
  ePiggyId <- elements piggyIds
  eAssetId <- arbitrary
  AssetSold eId ePiggyId eAssetId <$> arbitrary

arbitraryAssetValueChanged :: Gen Event
arbitraryAssetValueChanged = do
  eId <- arbitrary
  eAssetId <- arbitrary
  AssetValueChanged eId eAssetId <$> arbitrary

getEventId :: Event -> EventId
getEventId (AddedToPiggy eventId _ _) = eventId
getEventId (TakenFromPiggy eventId _ _) = eventId
getEventId (MovedBetweenPiggies eventId _ _ _) = eventId
getEventId (AssetBought eventId _ _ _) = eventId
getEventId (AssetSold eventId _ _ _) = eventId
getEventId (AssetValueChanged eventId _ _) = eventId

type PiggyId = Id Piggy

type FromPiggy = PiggyId

type ToPiggy = PiggyId

type EventId = Id Event

type AssetId = Id ()

type Shares = NonZero.NonZero Double

type Percentage = NonZero.NonZero Double
