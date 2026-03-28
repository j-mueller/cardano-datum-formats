{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Indigo.IAsset (
    IAssetDatum (..),
    IAssetPrice (..),
    IAssetContent (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions, sumOptions)
import Cardano.Protocol.Indigo.Common (AssetClass, OnChainDecimal, arbitraryAnyBuiltinByteString, maybeFromOptionData, maybeToOptionData)
import Cardano.Protocol.Indigo.PriceOracle (OracleAssetNft)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data IAssetPrice
    = Delisted OnChainDecimal
    | Oracle OracleAssetNft
    deriving stock (Eq, Show, Generic)

data IAssetContent = IAssetContent
    { iacAssetName :: PlutusTx.BuiltinByteString
    , iacPrice :: IAssetPrice
    , iacInterestOracleNft :: AssetClass
    , iacRedemptionRatio :: OnChainDecimal
    , iacMaintenanceRatio :: OnChainDecimal
    , iacLiquidationRatio :: OnChainDecimal
    , iacDebtMintingFeePercentage :: OnChainDecimal
    , iacLiquidationProcessingFeePercentage :: OnChainDecimal
    , iacStabilityPoolWithdrawalFeePercentage :: OnChainDecimal
    , iacRedemptionReimbursementPercentage :: OnChainDecimal
    , iacRedemptionProcessingFeePercentage :: OnChainDecimal
    , iacInterestCollectorPortionPercentage :: OnChainDecimal
    , iacFirstIAsset :: Bool
    , iacNextIAsset :: Maybe PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Show, Generic)

newtype IAssetDatum = IAssetDatum
    { getIAssetContent :: IAssetContent
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary IAssetPrice where
    arbitrary = Gen.oneof [Delisted <$> arbitrary, Oracle <$> arbitrary]

instance Arbitrary IAssetContent where
    arbitrary =
        IAssetContent
            <$> arbitraryAnyBuiltinByteString
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryMaybeBytes

instance Arbitrary IAssetDatum where
    arbitrary = IAssetDatum <$> arbitrary

instance PTx.ToData IAssetPrice where
    toBuiltinData = \case
        Delisted value -> PlutusTx.mkConstr 0 [PTx.toBuiltinData value]
        Oracle nft -> PlutusTx.mkConstr 1 [PTx.toBuiltinData nft]

instance PTx.FromData IAssetPrice where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [value]) -> Delisted <$> PTx.fromBuiltinData value
        (1, [nft]) -> Oracle <$> PTx.fromBuiltinData nft
        _ -> Nothing

instance PTx.ToData IAssetContent where
    toBuiltinData IAssetContent{iacAssetName, iacPrice, iacInterestOracleNft, iacRedemptionRatio, iacMaintenanceRatio, iacLiquidationRatio, iacDebtMintingFeePercentage, iacLiquidationProcessingFeePercentage, iacStabilityPoolWithdrawalFeePercentage, iacRedemptionReimbursementPercentage, iacRedemptionProcessingFeePercentage, iacInterestCollectorPortionPercentage, iacFirstIAsset, iacNextIAsset} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkB iacAssetName
            , PTx.toBuiltinData iacPrice
            , PTx.toBuiltinData iacInterestOracleNft
            , PTx.toBuiltinData iacRedemptionRatio
            , PTx.toBuiltinData iacMaintenanceRatio
            , PTx.toBuiltinData iacLiquidationRatio
            , PTx.toBuiltinData iacDebtMintingFeePercentage
            , PTx.toBuiltinData iacLiquidationProcessingFeePercentage
            , PTx.toBuiltinData iacStabilityPoolWithdrawalFeePercentage
            , PTx.toBuiltinData iacRedemptionReimbursementPercentage
            , PTx.toBuiltinData iacRedemptionProcessingFeePercentage
            , PTx.toBuiltinData iacInterestCollectorPortionPercentage
            , boolToData iacFirstIAsset
            , maybeToOptionDataBytes iacNextIAsset
            ]

instance PTx.FromData IAssetContent where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just iacAssetName, price, interestOracleNft, redemptionRatio, maintenanceRatio, liquidationRatio, debtMintingFeePercentage, liquidationProcessingFeePercentage, stabilityPoolWithdrawalFeePercentage, redemptionReimbursementPercentage, redemptionProcessingFeePercentage, interestCollectorPortionPercentage, firstIAsset, nextIAsset]) ->
            IAssetContent
                <$> pure iacAssetName
                <*> PTx.fromBuiltinData price
                <*> PTx.fromBuiltinData interestOracleNft
                <*> PTx.fromBuiltinData redemptionRatio
                <*> PTx.fromBuiltinData maintenanceRatio
                <*> PTx.fromBuiltinData liquidationRatio
                <*> PTx.fromBuiltinData debtMintingFeePercentage
                <*> PTx.fromBuiltinData liquidationProcessingFeePercentage
                <*> PTx.fromBuiltinData stabilityPoolWithdrawalFeePercentage
                <*> PTx.fromBuiltinData redemptionReimbursementPercentage
                <*> PTx.fromBuiltinData redemptionProcessingFeePercentage
                <*> PTx.fromBuiltinData interestCollectorPortionPercentage
                <*> boolFromData firstIAsset
                <*> maybeFromOptionData D.getB nextIAsset
        _ -> Nothing

instance PTx.ToData IAssetDatum where
    toBuiltinData (IAssetDatum content) =
        PlutusTx.mkConstr 1 [iassetContentToVariantData content]

instance PTx.FromData IAssetDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (1, [content]) -> IAssetDatum <$> iassetContentFromVariantData content
        _ -> Nothing

maybeToOptionDataBytes :: Maybe PlutusTx.BuiltinByteString -> PlutusTx.BuiltinData
maybeToOptionDataBytes = maybeToOptionData . fmap BytesAsData

newtype BytesAsData = BytesAsData { unBytesAsData :: PlutusTx.BuiltinByteString }

instance PTx.ToData BytesAsData where
    toBuiltinData = PlutusTx.mkB . unBytesAsData

arbitraryMaybeBytes :: Gen.Gen (Maybe PlutusTx.BuiltinByteString)
arbitraryMaybeBytes = Gen.oneof [pure Nothing, Just <$> arbitraryAnyBuiltinByteString]

boolToData :: Bool -> PlutusTx.BuiltinData
boolToData = \case
    False -> PlutusTx.mkConstr 0 []
    True -> PlutusTx.mkConstr 1 []

boolFromData :: PlutusTx.BuiltinData -> Maybe Bool
boolFromData dt = D.withConstr dt $ \case
    (0, []) -> Just False
    (1, []) -> Just True
    _ -> Nothing

iassetContentToVariantData :: IAssetContent -> PlutusTx.BuiltinData
iassetContentToVariantData IAssetContent{iacAssetName, iacPrice, iacInterestOracleNft, iacRedemptionRatio, iacMaintenanceRatio, iacLiquidationRatio, iacDebtMintingFeePercentage, iacLiquidationProcessingFeePercentage, iacStabilityPoolWithdrawalFeePercentage, iacRedemptionReimbursementPercentage, iacRedemptionProcessingFeePercentage, iacInterestCollectorPortionPercentage, iacFirstIAsset, iacNextIAsset} =
    PlutusTx.mkConstr
        0
        [ PlutusTx.mkB iacAssetName
        , PTx.toBuiltinData iacPrice
        , PTx.toBuiltinData iacInterestOracleNft
        , PTx.toBuiltinData iacRedemptionRatio
        , PTx.toBuiltinData iacMaintenanceRatio
        , PTx.toBuiltinData iacLiquidationRatio
        , PTx.toBuiltinData iacDebtMintingFeePercentage
        , PTx.toBuiltinData iacLiquidationProcessingFeePercentage
        , PTx.toBuiltinData iacStabilityPoolWithdrawalFeePercentage
        , PTx.toBuiltinData iacRedemptionReimbursementPercentage
        , PTx.toBuiltinData iacRedemptionProcessingFeePercentage
        , PTx.toBuiltinData iacInterestCollectorPortionPercentage
        , boolToData iacFirstIAsset
        , maybeToOptionDataBytes iacNextIAsset
        ]

iassetContentFromVariantData :: PlutusTx.BuiltinData -> Maybe IAssetContent
iassetContentFromVariantData dt = D.withConstr dt $ \case
    (0, [D.getB -> Just iacAssetName, price, interestOracleNft, redemptionRatio, maintenanceRatio, liquidationRatio, debtMintingFeePercentage, liquidationProcessingFeePercentage, stabilityPoolWithdrawalFeePercentage, redemptionReimbursementPercentage, redemptionProcessingFeePercentage, interestCollectorPortionPercentage, firstIAsset, nextIAsset]) ->
        IAssetContent
            <$> pure iacAssetName
            <*> PTx.fromBuiltinData price
            <*> PTx.fromBuiltinData interestOracleNft
            <*> PTx.fromBuiltinData redemptionRatio
            <*> PTx.fromBuiltinData maintenanceRatio
            <*> PTx.fromBuiltinData liquidationRatio
            <*> PTx.fromBuiltinData debtMintingFeePercentage
            <*> PTx.fromBuiltinData liquidationProcessingFeePercentage
            <*> PTx.fromBuiltinData stabilityPoolWithdrawalFeePercentage
            <*> PTx.fromBuiltinData redemptionReimbursementPercentage
            <*> PTx.fromBuiltinData redemptionProcessingFeePercentage
            <*> PTx.fromBuiltinData interestCollectorPortionPercentage
            <*> boolFromData firstIAsset
            <*> maybeFromOptionData D.getB nextIAsset
    _ -> Nothing

instance ToJSON IAssetPrice where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON IAssetPrice where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

$(deriveTypeScript (sumOptions 0) ''IAssetPrice)

instance Schema.ToSchema IAssetPrice where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

instance ToJSON IAssetContent where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON IAssetContent where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''IAssetContent)

instance Schema.ToSchema IAssetContent where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))

instance ToJSON IAssetDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON IAssetDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''IAssetDatum)

instance Schema.ToSchema IAssetDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))
