{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Indigo.PriceOracle (
    OracleAssetNft (..),
    PriceOracleDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.Indigo.Common (AssetClass, OnChainDecimal)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

newtype OracleAssetNft = OracleAssetNft
    { oanAsset :: AssetClass
    }
    deriving stock (Eq, Show, Generic)

data PriceOracleDatum = PriceOracleDatum
    { podPrice :: OnChainDecimal
    , podExpiration :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary OracleAssetNft where
    arbitrary = OracleAssetNft <$> arbitrary

instance Arbitrary PriceOracleDatum where
    arbitrary = PriceOracleDatum <$> arbitrary <*> arbitrary

instance PTx.ToData OracleAssetNft where
    toBuiltinData OracleAssetNft{oanAsset} =
        PlutusTx.mkConstr 0 [PTx.toBuiltinData oanAsset]

instance PTx.FromData OracleAssetNft where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [asset]) ->
            OracleAssetNft <$> PTx.fromBuiltinData asset
        _ -> Nothing

instance PTx.ToData PriceOracleDatum where
    toBuiltinData PriceOracleDatum{podPrice, podExpiration} =
        PlutusTx.mkConstr 0 [PTx.toBuiltinData podPrice, PlutusTx.mkI podExpiration]

instance PTx.FromData PriceOracleDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [price, D.getI -> Just podExpiration]) ->
            PriceOracleDatum <$> PTx.fromBuiltinData price <*> pure podExpiration
        _ -> Nothing

instance ToJSON OracleAssetNft where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON OracleAssetNft where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''OracleAssetNft)

instance Schema.ToSchema OracleAssetNft where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))

instance ToJSON PriceOracleDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON PriceOracleDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''PriceOracleDatum)

instance Schema.ToSchema PriceOracleDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))
