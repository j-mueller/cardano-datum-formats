{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Strike.Pool (
    PoolDatum (..),
) where

import Cardano.Asset (Asset)
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.JSON ()
import Cardano.Protocol.Strike.Common (arbitraryAnyBuiltinByteString)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data PoolDatum = PoolDatum
    { pdUnderlyingAsset :: Asset
    , pdLpAsset :: Asset
    , pdLiquidityTotalAssetAmount :: Integer
    , pdLiquidityTotalLpMinted :: Integer
    , pdTotalLendedAmount :: Integer
    , pdBatcherLicense :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary PoolDatum where
    arbitrary =
        PoolDatum
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryAnyBuiltinByteString

instance PTx.ToData PoolDatum where
    toBuiltinData PoolDatum{pdUnderlyingAsset, pdLpAsset, pdLiquidityTotalAssetAmount, pdLiquidityTotalLpMinted, pdTotalLendedAmount, pdBatcherLicense} =
        PlutusTx.mkConstr
            0
            [ PTx.toBuiltinData pdUnderlyingAsset
            , PTx.toBuiltinData pdLpAsset
            , PlutusTx.mkI pdLiquidityTotalAssetAmount
            , PlutusTx.mkI pdLiquidityTotalLpMinted
            , PlutusTx.mkI pdTotalLendedAmount
            , PlutusTx.mkB pdBatcherLicense
            ]

instance PTx.FromData PoolDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [underlyingAsset, lpAsset, D.getI -> Just pdLiquidityTotalAssetAmount, D.getI -> Just pdLiquidityTotalLpMinted, D.getI -> Just pdTotalLendedAmount, D.getB -> Just pdBatcherLicense]) ->
            PoolDatum
                <$> PTx.fromBuiltinData underlyingAsset
                <*> PTx.fromBuiltinData lpAsset
                <*> pure pdLiquidityTotalAssetAmount
                <*> pure pdLiquidityTotalLpMinted
                <*> pure pdTotalLendedAmount
                <*> pure pdBatcherLicense
        _ -> Nothing

instance ToJSON PoolDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON PoolDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''PoolDatum)

instance Schema.ToSchema PoolDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
