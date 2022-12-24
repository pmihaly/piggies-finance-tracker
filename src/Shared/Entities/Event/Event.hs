{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Shared.Entities.Event.Event (Event (..)) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Aeson.Key (fromString)
import GHC.Generics (Generic)
import PiggyBalance.Entities.Piggy (Piggy)
import Shared.ValueObjects.Id (Id)
import Shared.ValueObjects.Money (Money)
import Shared.ValueObjects.NonZero qualified as NonZero
import Shared.ValueObjects.Positive (Positive)

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

type PiggyId = Id Piggy

type FromPiggy = PiggyId

type ToPiggy = PiggyId

type EventId = Id Event

type AssetId = Id ()

type Shares = NonZero.NonZero Double

type Percentage = NonZero.NonZero Double
