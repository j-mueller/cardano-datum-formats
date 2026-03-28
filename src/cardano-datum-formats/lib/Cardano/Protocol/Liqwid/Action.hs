{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Liqwid.Action (
    ActionDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.Liqwid.Common (BatchState, fromList)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data ActionDatum = ActionDatum
    { adBatchState :: BatchState
    , adReservedSupply :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary ActionDatum where
    arbitrary = ActionDatum <$> arbitrary <*> arbitrary

instance PTx.ToData ActionDatum where
    toBuiltinData ActionDatum{adBatchState, adReservedSupply} =
        PlutusTx.mkList [PTx.toBuiltinData adBatchState, PlutusTx.mkI adReservedSupply]

instance PTx.FromData ActionDatum where
    fromBuiltinData dt = do
        [batchState, D.getI -> Just adReservedSupply] <- fromList dt
        adBatchState <- PTx.fromBuiltinData batchState
        pure ActionDatum{adBatchState, adReservedSupply}

instance ToJSON ActionDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON ActionDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''ActionDatum)

instance Schema.ToSchema ActionDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
