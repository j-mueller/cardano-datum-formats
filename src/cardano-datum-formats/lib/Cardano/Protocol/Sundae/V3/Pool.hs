{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Sundae.V3.Pool (
    FeeBasisPoints (..),
    ProtocolFeeAmounts (..),
    SundaePoolDatum (..),
    SundaeStablePoolDatum (..),
) where

import Cardano.Api qualified as C
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.Sundae.V3.Order (MultisigScript)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.ByteString qualified as BS
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Hedgehog qualified as H

data FeeBasisPoints = FeeBasisPoints
    { fbpBid :: Integer
    , fbpAsk :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary FeeBasisPoints where
    arbitrary =
        FeeBasisPoints
            <$> arbitrary
            <*> arbitrary

instance PlutusTx.ToData FeeBasisPoints where
    toBuiltinData FeeBasisPoints{fbpBid, fbpAsk} =
        PlutusTx.mkList [PlutusTx.mkI fbpBid, PlutusTx.mkI fbpAsk]

instance PlutusTx.FromData FeeBasisPoints where
    fromBuiltinData dt = withList dt $ \case
        [D.getI -> Just fbpBid, D.getI -> Just fbpAsk] ->
            Just FeeBasisPoints{fbpBid, fbpAsk}
        _ -> Nothing

data ProtocolFeeAmounts = ProtocolFeeAmounts
    { pfaFirst :: C.Quantity
    , pfaSecond :: C.Quantity
    , pfaThird :: C.Quantity
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary ProtocolFeeAmounts where
    arbitrary =
        ProtocolFeeAmounts
            <$> H.hedgehog Gen.genPositiveQuantity
            <*> H.hedgehog Gen.genPositiveQuantity
            <*> H.hedgehog Gen.genPositiveQuantity

instance PlutusTx.ToData ProtocolFeeAmounts where
    toBuiltinData ProtocolFeeAmounts{pfaFirst = C.Quantity pfaFirst, pfaSecond = C.Quantity pfaSecond, pfaThird = C.Quantity pfaThird} =
        PlutusTx.mkList [PlutusTx.mkI pfaFirst, PlutusTx.mkI pfaSecond, PlutusTx.mkI pfaThird]

instance PlutusTx.FromData ProtocolFeeAmounts where
    fromBuiltinData dt = withList dt $ \case
        [D.getI -> Just pfaFirst, D.getI -> Just pfaSecond, D.getI -> Just pfaThird] ->
            Just
                ProtocolFeeAmounts
                    { pfaFirst = C.Quantity pfaFirst
                    , pfaSecond = C.Quantity pfaSecond
                    , pfaThird = C.Quantity pfaThird
                    }
        _ -> Nothing

data SundaePoolDatum = SundaePoolDatum
    { spdIdentifier :: PlutusTx.BuiltinByteString
    , spdAssetA :: C.AssetId
    , spdAssetB :: C.AssetId
    , spdCirculatingLp :: C.Quantity
    , spdBidFeesPer10Thousand :: Integer
    , spdAskFeesPer10Thousand :: Integer
    , spdFeeManager :: Maybe MultisigScript
    , spdMarketOpen :: Integer
    , spdProtocolFees :: C.Quantity
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary SundaePoolDatum where
    arbitrary =
        SundaePoolDatum
            <$> arbitraryIdentifier
            <*> H.hedgehog Gen.genAssetId
            <*> H.hedgehog Gen.genAssetId
            <*> H.hedgehog Gen.genPositiveQuantity
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> H.hedgehog Gen.genPositiveQuantity

instance PlutusTx.ToData SundaePoolDatum where
    toBuiltinData SundaePoolDatum{spdIdentifier, spdAssetA, spdAssetB, spdCirculatingLp = C.Quantity spdCirculatingLp, spdBidFeesPer10Thousand, spdAskFeesPer10Thousand, spdFeeManager, spdMarketOpen, spdProtocolFees = C.Quantity spdProtocolFees} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkB spdIdentifier
            , serialiseAssetPair spdAssetA spdAssetB
            , PlutusTx.mkI spdCirculatingLp
            , PlutusTx.mkI spdBidFeesPer10Thousand
            , PlutusTx.mkI spdAskFeesPer10Thousand
            , PlutusTx.toBuiltinData spdFeeManager
            , PlutusTx.mkI spdMarketOpen
            , PlutusTx.mkI spdProtocolFees
            ]

instance PlutusTx.FromData SundaePoolDatum where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [D.getB -> Just spdIdentifier, assets, circulatingLp, bidFees, askFees, feeManager, marketOpen, protocolFees]) -> do
            (spdAssetA, spdAssetB) <- deserialiseAssetPair assets
            SundaePoolDatum
                <$> pure spdIdentifier
                <*> pure spdAssetA
                <*> pure spdAssetB
                <*> fmap C.Quantity (D.getI circulatingLp)
                <*> D.getI bidFees
                <*> D.getI askFees
                <*> PlutusTx.fromBuiltinData feeManager
                <*> D.getI marketOpen
                <*> fmap C.Quantity (D.getI protocolFees)
        _ -> Nothing

data SundaeStablePoolDatum = SundaeStablePoolDatum
    { sspdIdentifier :: PlutusTx.BuiltinByteString
    , sspdAssetA :: C.AssetId
    , sspdAssetB :: C.AssetId
    , sspdCirculatingLp :: C.Quantity
    , sspdLpFeeBasisPoints :: FeeBasisPoints
    , sspdProtocolFeeBasisPoints :: FeeBasisPoints
    , sspdFeeManager :: Maybe MultisigScript
    , sspdMarketOpen :: Integer
    , sspdProtocolFees :: ProtocolFeeAmounts
    , sspdLinearAmplification :: Integer
    , sspdSumInvariant :: Integer
    , sspdLinearAmplificationManager :: Maybe MultisigScript
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary SundaeStablePoolDatum where
    arbitrary =
        SundaeStablePoolDatum
            <$> arbitraryIdentifier
            <*> H.hedgehog Gen.genAssetId
            <*> H.hedgehog Gen.genAssetId
            <*> H.hedgehog Gen.genPositiveQuantity
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance PlutusTx.ToData SundaeStablePoolDatum where
    toBuiltinData SundaeStablePoolDatum{sspdIdentifier, sspdAssetA, sspdAssetB, sspdCirculatingLp = C.Quantity sspdCirculatingLp, sspdLpFeeBasisPoints, sspdProtocolFeeBasisPoints, sspdFeeManager, sspdMarketOpen, sspdProtocolFees, sspdLinearAmplification, sspdSumInvariant, sspdLinearAmplificationManager} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkB sspdIdentifier
            , serialiseAssetPair sspdAssetA sspdAssetB
            , PlutusTx.mkI sspdCirculatingLp
            , PlutusTx.toBuiltinData sspdLpFeeBasisPoints
            , PlutusTx.toBuiltinData sspdProtocolFeeBasisPoints
            , PlutusTx.toBuiltinData sspdFeeManager
            , PlutusTx.mkI sspdMarketOpen
            , PlutusTx.toBuiltinData sspdProtocolFees
            , PlutusTx.mkI sspdLinearAmplification
            , PlutusTx.mkI sspdSumInvariant
            , PlutusTx.toBuiltinData sspdLinearAmplificationManager
            ]

instance PlutusTx.FromData SundaeStablePoolDatum where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [D.getB -> Just sspdIdentifier, assets, circulatingLp, lpFeeBasisPoints, protocolFeeBasisPoints, feeManager, marketOpen, protocolFees, linearAmplification, sumInvariant, linearAmplificationManager]) -> do
            (sspdAssetA, sspdAssetB) <- deserialiseAssetPair assets
            SundaeStablePoolDatum
                <$> pure sspdIdentifier
                <*> pure sspdAssetA
                <*> pure sspdAssetB
                <*> fmap C.Quantity (D.getI circulatingLp)
                <*> PlutusTx.fromBuiltinData lpFeeBasisPoints
                <*> PlutusTx.fromBuiltinData protocolFeeBasisPoints
                <*> PlutusTx.fromBuiltinData feeManager
                <*> D.getI marketOpen
                <*> PlutusTx.fromBuiltinData protocolFees
                <*> D.getI linearAmplification
                <*> D.getI sumInvariant
                <*> PlutusTx.fromBuiltinData linearAmplificationManager
        _ -> Nothing

arbitraryIdentifier :: QC.Gen PlutusTx.BuiltinByteString
arbitraryIdentifier =
    PlutusTx.toBuiltin . C.serialiseToRawBytes <$> H.hedgehog Gen.genScriptHash

serialiseAssetPair :: C.AssetId -> C.AssetId -> PlutusTx.BuiltinData
serialiseAssetPair assetA assetB =
    PlutusTx.mkList [serialiseAsset assetA, serialiseAsset assetB]

deserialiseAssetPair :: PlutusTx.BuiltinData -> Maybe (C.AssetId, C.AssetId)
deserialiseAssetPair dt = withList dt $ \case
    [assetA, assetB] ->
        (,) <$> deserialiseAsset assetA <*> deserialiseAsset assetB
    _ -> Nothing

serialiseAsset :: C.AssetId -> PlutusTx.BuiltinData
serialiseAsset = \case
    C.AdaAssetId -> PlutusTx.mkList [PlutusTx.mkB (PlutusTx.toBuiltin BS.empty), PlutusTx.mkB (PlutusTx.toBuiltin BS.empty)]
    C.AssetId policyId assetName ->
        PlutusTx.mkList
            [ PlutusTx.mkB (PlutusTx.toBuiltin $ C.serialiseToRawBytes policyId)
            , PlutusTx.mkB (PlutusTx.toBuiltin $ C.serialiseToRawBytes assetName)
            ]

deserialiseAsset :: PlutusTx.BuiltinData -> Maybe C.AssetId
deserialiseAsset dt = withList dt $ \case
    [D.getB -> Just policyId, D.getB -> Just assetName]
        | BS.null (PlutusTx.fromBuiltin policyId) && BS.null (PlutusTx.fromBuiltin assetName) ->
            Just C.AdaAssetId
        | otherwise ->
            C.AssetId
                <$> either (const Nothing) Just (C.deserialiseFromRawBytes C.AsPolicyId $ PlutusTx.fromBuiltin policyId)
                <*> either (const Nothing) Just (C.deserialiseFromRawBytes C.AsAssetName $ PlutusTx.fromBuiltin assetName)
    _ -> Nothing

withList :: PlutusTx.BuiltinData -> ([PlutusTx.BuiltinData] -> Maybe a) -> Maybe a
withList dt match =
    PlutusTx.matchData
        dt
        (\_ _ -> Nothing)
        (const Nothing)
        match
        (const Nothing)
        (const Nothing)

instance ToJSON FeeBasisPoints where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON FeeBasisPoints where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''FeeBasisPoints)

instance Schema.ToSchema FeeBasisPoints where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))

instance ToJSON ProtocolFeeAmounts where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON ProtocolFeeAmounts where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''ProtocolFeeAmounts)

instance Schema.ToSchema ProtocolFeeAmounts where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))

instance ToJSON SundaePoolDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON SundaePoolDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''SundaePoolDatum)

instance Schema.ToSchema SundaePoolDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))

instance ToJSON SundaeStablePoolDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 4)
    toEncoding = Aeson.genericToEncoding (jsonOptions 4)

instance FromJSON SundaeStablePoolDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 4)

$(deriveTypeScript (jsonOptions 4) ''SundaeStablePoolDatum)

instance Schema.ToSchema SundaeStablePoolDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 4))
