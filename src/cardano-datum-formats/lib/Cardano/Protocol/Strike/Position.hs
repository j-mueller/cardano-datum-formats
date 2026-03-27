{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Strike.Position (
    PositionDatum (..),
) where

import Cardano.Asset (Asset)
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.JSON ()
import Cardano.Protocol.Strike.Common (
    AddressHash,
    PositionSide,
    ScriptHash,
    arbitraryAnyBuiltinByteString,
    maybeFromOptionData,
    maybeToOptionData,
 )
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data PositionDatum = PositionDatum
    { pdOwnerPkh :: AddressHash
    , pdOwnerStakeKey :: Maybe AddressHash
    , pdEnteredPositionTime :: Integer
    , pdEnteredAtUsdPrice :: Integer
    , pdPositionPolicyId :: PlutusTx.BuiltinByteString
    , pdManagePositionsScriptHash :: ScriptHash
    , pdCollateralAsset :: Asset
    , pdMaintainMarginAmount :: Integer
    , pdHourlyUsdBorrowFee :: Integer
    , pdStopLossUsdPrice :: Integer
    , pdTakeProfitUsdPrice :: Integer
    , pdCollateralAssetAmount :: Integer
    , pdPositionAssetAmount :: Integer
    , pdSide :: PositionSide
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary PositionDatum where
    arbitrary =
        PositionDatum
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryAnyBuiltinByteString
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance PTx.ToData PositionDatum where
    toBuiltinData
        PositionDatum
            { pdOwnerPkh
            , pdOwnerStakeKey
            , pdEnteredPositionTime
            , pdEnteredAtUsdPrice
            , pdPositionPolicyId
            , pdManagePositionsScriptHash
            , pdCollateralAsset
            , pdMaintainMarginAmount
            , pdHourlyUsdBorrowFee
            , pdStopLossUsdPrice
            , pdTakeProfitUsdPrice
            , pdCollateralAssetAmount
            , pdPositionAssetAmount
            , pdSide
            } =
            PlutusTx.mkConstr
                0
                [ PTx.toBuiltinData pdOwnerPkh
                , maybeToOptionData pdOwnerStakeKey
                , PlutusTx.mkI pdEnteredPositionTime
                , PlutusTx.mkI pdEnteredAtUsdPrice
                , PlutusTx.mkB pdPositionPolicyId
                , PTx.toBuiltinData pdManagePositionsScriptHash
                , PTx.toBuiltinData pdCollateralAsset
                , PlutusTx.mkI pdMaintainMarginAmount
                , PlutusTx.mkI pdHourlyUsdBorrowFee
                , PlutusTx.mkI pdStopLossUsdPrice
                , PlutusTx.mkI pdTakeProfitUsdPrice
                , PlutusTx.mkI pdCollateralAssetAmount
                , PlutusTx.mkI pdPositionAssetAmount
                , PTx.toBuiltinData pdSide
                ]

instance PTx.FromData PositionDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [ ownerPkh
            , ownerStakeKey
            , D.getI -> Just pdEnteredPositionTime
            , D.getI -> Just pdEnteredAtUsdPrice
            , D.getB -> Just pdPositionPolicyId
            , managePositionsScriptHash
            , collateralAsset
            , D.getI -> Just pdMaintainMarginAmount
            , D.getI -> Just pdHourlyUsdBorrowFee
            , D.getI -> Just pdStopLossUsdPrice
            , D.getI -> Just pdTakeProfitUsdPrice
            , D.getI -> Just pdCollateralAssetAmount
            , D.getI -> Just pdPositionAssetAmount
            , side
            ]) ->
                PositionDatum
                    <$> PTx.fromBuiltinData ownerPkh
                    <*> maybeFromOptionData PTx.fromBuiltinData ownerStakeKey
                    <*> pure pdEnteredPositionTime
                    <*> pure pdEnteredAtUsdPrice
                    <*> pure pdPositionPolicyId
                    <*> PTx.fromBuiltinData managePositionsScriptHash
                    <*> PTx.fromBuiltinData collateralAsset
                    <*> pure pdMaintainMarginAmount
                    <*> pure pdHourlyUsdBorrowFee
                    <*> pure pdStopLossUsdPrice
                    <*> pure pdTakeProfitUsdPrice
                    <*> pure pdCollateralAssetAmount
                    <*> pure pdPositionAssetAmount
                    <*> PTx.fromBuiltinData side
        _ -> Nothing

instance ToJSON PositionDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON PositionDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''PositionDatum)

instance Schema.ToSchema PositionDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
