{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Pulse.MarketInfo (
    MarketInfoDatum (..),
) where

import Cardano.Asset (Asset)
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.JSON ()
import Cardano.Transaction.OutputReference (OutputReference)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.ByteString qualified as BS
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data MarketInfoDatum = MarketInfoDatum
    { midMarketId :: OutputReference
    , midUnderlyingAsset :: Asset
    , midMinLiquidity :: Integer
    , midPtTokenName :: PlutusTx.BuiltinByteString
    , midYtTokenName :: PlutusTx.BuiltinByteString
    , midExpiry :: Integer
    , midDuration :: Integer
    , midMultiplier :: Integer
    , midBatcherPolicyId :: PlutusTx.BuiltinByteString
    , midOrderScriptHash :: PlutusTx.BuiltinByteString
    , midSyScriptHash :: PlutusTx.BuiltinByteString
    , midYtStakeScriptHash :: PlutusTx.BuiltinByteString
    , midYtStakeAuthTokenPolicyId :: PlutusTx.BuiltinByteString
    , midYieldTokenizationPolicyId :: PlutusTx.BuiltinByteString
    , midLpTokenAsset :: Asset
    , midOracleNft :: Asset
    , midEnvelopeAmount :: Integer
    , midScalarRoot :: Integer
    , midInitialAnchor :: Integer
    , midLnFeeRateRoot :: Integer
    , midInitPyIndex :: Integer
    , midLnBorrowFeeRate :: Integer
    , midYtRewardFeeRate :: Integer
    , midLpRevenueFeeRate :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary MarketInfoDatum where
    arbitrary =
        MarketInfoDatum
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryBuiltinByteString
            <*> arbitraryBuiltinByteString
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryBuiltinByteString
            <*> arbitraryBuiltinByteString
            <*> arbitraryBuiltinByteString
            <*> arbitraryBuiltinByteString
            <*> arbitraryBuiltinByteString
            <*> arbitraryBuiltinByteString
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

instance PTx.ToData MarketInfoDatum where
    toBuiltinData MarketInfoDatum{midMarketId, midUnderlyingAsset, midMinLiquidity, midPtTokenName, midYtTokenName, midExpiry, midDuration, midMultiplier, midBatcherPolicyId, midOrderScriptHash, midSyScriptHash, midYtStakeScriptHash, midYtStakeAuthTokenPolicyId, midYieldTokenizationPolicyId, midLpTokenAsset, midOracleNft, midEnvelopeAmount, midScalarRoot, midInitialAnchor, midLnFeeRateRoot, midInitPyIndex, midLnBorrowFeeRate, midYtRewardFeeRate, midLpRevenueFeeRate} =
        PlutusTx.mkConstr
            0
            [ PTx.toBuiltinData midMarketId
            , PTx.toBuiltinData midUnderlyingAsset
            , PlutusTx.mkI midMinLiquidity
            , PlutusTx.mkB midPtTokenName
            , PlutusTx.mkB midYtTokenName
            , PlutusTx.mkI midExpiry
            , PlutusTx.mkI midDuration
            , PlutusTx.mkI midMultiplier
            , PlutusTx.mkB midBatcherPolicyId
            , PlutusTx.mkB midOrderScriptHash
            , PlutusTx.mkB midSyScriptHash
            , PlutusTx.mkB midYtStakeScriptHash
            , PlutusTx.mkB midYtStakeAuthTokenPolicyId
            , PlutusTx.mkB midYieldTokenizationPolicyId
            , PTx.toBuiltinData midLpTokenAsset
            , PTx.toBuiltinData midOracleNft
            , PlutusTx.mkI midEnvelopeAmount
            , PlutusTx.mkI midScalarRoot
            , PlutusTx.mkI midInitialAnchor
            , PlutusTx.mkI midLnFeeRateRoot
            , PlutusTx.mkI midInitPyIndex
            , PlutusTx.mkI midLnBorrowFeeRate
            , PlutusTx.mkI midYtRewardFeeRate
            , PlutusTx.mkI midLpRevenueFeeRate
            ]

instance PTx.FromData MarketInfoDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [marketId, underlyingAsset, D.getI -> Just midMinLiquidity, D.getB -> Just midPtTokenName, D.getB -> Just midYtTokenName, D.getI -> Just midExpiry, D.getI -> Just midDuration, D.getI -> Just midMultiplier, D.getB -> Just midBatcherPolicyId, D.getB -> Just midOrderScriptHash, D.getB -> Just midSyScriptHash, D.getB -> Just midYtStakeScriptHash, D.getB -> Just midYtStakeAuthTokenPolicyId, D.getB -> Just midYieldTokenizationPolicyId, lpTokenAsset, oracleNft, D.getI -> Just midEnvelopeAmount, D.getI -> Just midScalarRoot, D.getI -> Just midInitialAnchor, D.getI -> Just midLnFeeRateRoot, D.getI -> Just midInitPyIndex, D.getI -> Just midLnBorrowFeeRate, D.getI -> Just midYtRewardFeeRate, D.getI -> Just midLpRevenueFeeRate]) ->
            MarketInfoDatum
                <$> PTx.fromBuiltinData marketId
                <*> PTx.fromBuiltinData underlyingAsset
                <*> pure midMinLiquidity
                <*> pure midPtTokenName
                <*> pure midYtTokenName
                <*> pure midExpiry
                <*> pure midDuration
                <*> pure midMultiplier
                <*> pure midBatcherPolicyId
                <*> pure midOrderScriptHash
                <*> pure midSyScriptHash
                <*> pure midYtStakeScriptHash
                <*> pure midYtStakeAuthTokenPolicyId
                <*> pure midYieldTokenizationPolicyId
                <*> PTx.fromBuiltinData lpTokenAsset
                <*> PTx.fromBuiltinData oracleNft
                <*> pure midEnvelopeAmount
                <*> pure midScalarRoot
                <*> pure midInitialAnchor
                <*> pure midLnFeeRateRoot
                <*> pure midInitPyIndex
                <*> pure midLnBorrowFeeRate
                <*> pure midYtRewardFeeRate
                <*> pure midLpRevenueFeeRate
        _ -> Nothing

arbitraryBuiltinByteString :: Gen.Gen PlutusTx.BuiltinByteString
arbitraryBuiltinByteString =
    PlutusTx.toBuiltin . BS.pack <$> Gen.listOf arbitrary

instance ToJSON MarketInfoDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON MarketInfoDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''MarketInfoDatum)

instance Schema.ToSchema MarketInfoDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))
