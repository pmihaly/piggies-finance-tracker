{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Shared.Entities.Event.Event (Event (..), getEventId) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Key (fromString, toString)
import GHC.Generics (Generic)
import PiggyBalance.Entities.Piggy (Piggy)
import Shared.ValueObjects.Available (Available)
import Shared.ValueObjects.Id (Id)
import Shared.ValueObjects.Money (Money)
import Shared.ValueObjects.NonZero qualified as NonZero
import Shared.ValueObjects.Positive (Positive)

data Event
  = AddedToPiggy EventId ToPiggy (NonZero.NonZero (Positive Money))
  | TakenFromPiggy EventId FromPiggy (Available (NonZero.NonZero (Positive Money)))
  | MovedBetweenPiggies EventId FromPiggy ToPiggy (Available (NonZero.NonZero (Positive Money)))
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

instance ToJSON Event where
  toJSON (AddedToPiggy eId eToPiggy eAmount) =
    object ["type" .= toString "added-to-piggy", "id" .= eId, "to-piggy" .= eToPiggy, "amount" .= eAmount]
  toJSON (TakenFromPiggy eId eFromPiggy eAmount) =
    object ["type" .= toString "taken-from-piggy", "id" .= eId, "from-piggy" .= eFromPiggy, "amount" .= eAmount]
  toJSON (MovedBetweenPiggies eId eFromPiggy eToPiggy eAmount) =
    object ["type" .= toString "moved-between-piggies", "id" .= eId, "from-piggy" .= eFromPiggy, "to-piggy" .= eToPiggy, "amount" .= eAmount]
  toJSON (AssetBought eId ePiggyId eAssetId eShares) =
    object ["type" .= toString "asset-bought", "id" .= eId, "piggy" .= ePiggyId, "name" .= eAssetId, "shares" .= eShares]
  toJSON (AssetSold eId ePiggyId eAssetId eShares) =
    object ["type" .= toString "asset-sold", "id" .= eId, "piggy" .= ePiggyId, "investment" .= eAssetId, "shares" .= eShares]
  toJSON (AssetValueChanged eId eAssetId ePercentage) =
    object ["type" .= toString "asset-value-changed", "id" .= eId, "name" .= eAssetId, "percentage" .= ePercentage]

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
