{-# LANGUAGE ImportQualifiedPost #-}

module PiggyBalance.PiggyBalances (PiggyBalances, getOneId) where

import Control.Category ((>>>))
import Data.HashMap.Strict qualified as Map
import PiggyBalance.Entities.Piggy (Piggy)
import Shared.ValueObjects.Id (Id)

type PiggyBalances = Map.HashMap (Id Piggy) Piggy

getOneId :: PiggyBalances -> Id Piggy
getOneId = Map.toList >>> head >>> fst
