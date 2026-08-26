{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Sundae.V4.Pool (
    Rational (..),
    ConstantSumConfig (..),
    AssetAmount (..),
    ActionEntry (..),
    ModuleState (..),
    PoolDatum (..),
) where

import Cardano.Api qualified as C
import Cardano.Asset (Asset)
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen
import Prelude hiding (Rational)

data AssetAmount = AssetAmount
    { aaAsset :: Asset
    , aaAmount :: C.Quantity
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary AssetAmount where
    arbitrary = AssetAmount <$> arbitrary <*> (C.Quantity <$> arbitrary)

instance PlutusTx.ToData AssetAmount where
    toBuiltinData AssetAmount{aaAsset, aaAmount = C.Quantity amount} =
        PlutusTx.mkList [PlutusTx.toBuiltinData aaAsset, PlutusTx.mkI amount]

instance PlutusTx.FromData AssetAmount where
    fromBuiltinData dt = D.fromList dt $ \case
        [asset, D.getI -> Just amount] -> AssetAmount <$> PlutusTx.fromBuiltinData asset <*> pure (C.Quantity amount)
        _ -> Nothing

data Rational = Rational
    { rNum :: Integer
    , rDen :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary Rational where
    arbitrary = Rational <$> arbitrary <*> arbitrary

instance PlutusTx.ToData Rational where
    toBuiltinData Rational{rNum, rDen} =
        PlutusTx.mkConstr 0 [PlutusTx.mkI rNum, PlutusTx.mkI rDen]

instance PlutusTx.FromData Rational where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just rNum, D.getI -> Just rDen]) ->
            Just Rational{rNum, rDen}
        _ -> Nothing

data ConstantSumConfig = ConstantSumConfig
    { cscPrices :: [Integer]
    , cscFee :: Rational
    , cscBountyK :: Rational
    , cscWaiveFeeOnClaim :: Bool
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary ConstantSumConfig where
    arbitrary = ConstantSumConfig <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance PlutusTx.ToData ConstantSumConfig where
    toBuiltinData ConstantSumConfig{cscPrices, cscFee, cscBountyK, cscWaiveFeeOnClaim} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.toBuiltinData cscPrices
            , PlutusTx.toBuiltinData cscFee
            , PlutusTx.toBuiltinData cscBountyK
            , PlutusTx.toBuiltinData cscWaiveFeeOnClaim
            ]

instance PlutusTx.FromData ConstantSumConfig where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [prices, fee, bountyK, waiveFeeOnClaim]) ->
            ConstantSumConfig
                <$> PlutusTx.fromBuiltinData prices
                <*> PlutusTx.fromBuiltinData fee
                <*> PlutusTx.fromBuiltinData bountyK
                <*> PlutusTx.fromBuiltinData waiveFeeOnClaim
        _ -> Nothing

data ActionEntry = ActionEntry
    { aeTag :: Integer
    , aeEnabled :: Bool
    , aeModules :: [PlutusTx.BuiltinByteString]
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary ActionEntry where
    arbitrary = ActionEntry <$> arbitrary <*> arbitrary <*> Gen.listOf D.arbitraryBuiltinByteString

instance PlutusTx.ToData ActionEntry where
    toBuiltinData ActionEntry{aeTag, aeEnabled, aeModules} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkI aeTag
            , PlutusTx.toBuiltinData aeEnabled
            , PlutusTx.toBuiltinData aeModules
            ]

instance PlutusTx.FromData ActionEntry where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just aeTag, enabled, modules]) ->
            ActionEntry aeTag <$> PlutusTx.fromBuiltinData enabled <*> PlutusTx.fromBuiltinData modules
        _ -> Nothing

data ModuleState = ModuleState
    { msModuleHash :: PlutusTx.BuiltinByteString
    , msState :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary ModuleState where
    arbitrary = ModuleState <$> D.arbitraryBuiltinByteString <*> D.arbitraryBuiltinByteString

instance PlutusTx.ToData ModuleState where
    toBuiltinData ModuleState{msModuleHash, msState} =
        PlutusTx.mkList [PlutusTx.mkB msModuleHash, PlutusTx.mkB msState]

instance PlutusTx.FromData ModuleState where
    fromBuiltinData dt = D.fromList dt $ \case
        [D.getB -> Just msModuleHash, D.getB -> Just msState] -> Just ModuleState{msModuleHash, msState}
        _ -> Nothing

data PoolDatum = PoolDatum
    { pdAssets :: [AssetAmount]
    , pdTotalLp :: C.Quantity
    , pdCirculatingLp :: C.Quantity
    , pdPremintedLp :: C.Quantity
    , pdIdentifier :: PlutusTx.BuiltinByteString
    , pdActions :: [ActionEntry]
    , pdModuleState :: [ModuleState]
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary PoolDatum where
    arbitrary =
        PoolDatum
            <$> arbitrary
            <*> (C.Quantity <$> arbitrary)
            <*> (C.Quantity <$> arbitrary)
            <*> (C.Quantity <$> arbitrary)
            <*> D.arbitraryBuiltinByteString
            <*> arbitrary
            <*> arbitrary

instance PlutusTx.ToData PoolDatum where
    toBuiltinData PoolDatum{pdAssets, pdTotalLp = C.Quantity totalLp, pdCirculatingLp = C.Quantity circulatingLp, pdPremintedLp = C.Quantity premintedLp, pdIdentifier, pdActions, pdModuleState} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.toBuiltinData pdAssets
            , PlutusTx.mkI totalLp
            , PlutusTx.mkI circulatingLp
            , PlutusTx.mkI premintedLp
            , PlutusTx.mkB pdIdentifier
            , PlutusTx.toBuiltinData pdActions
            , PlutusTx.toBuiltinData pdModuleState
            ]

instance PlutusTx.FromData PoolDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [assets, totalLp, circulatingLp, premintedLp, D.getB -> Just identifier, actions, moduleState]) ->
            PoolDatum
                <$> PlutusTx.fromBuiltinData assets
                <*> fmap C.Quantity (D.getI totalLp)
                <*> fmap C.Quantity (D.getI circulatingLp)
                <*> fmap C.Quantity (D.getI premintedLp)
                <*> pure identifier
                <*> PlutusTx.fromBuiltinData actions
                <*> PlutusTx.fromBuiltinData moduleState
        _ -> Nothing

instance ToJSON AssetAmount where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON AssetAmount where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''AssetAmount)

instance Schema.ToSchema AssetAmount where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSON Rational where
    toJSON = Aeson.genericToJSON (jsonOptions 1)
    toEncoding = Aeson.genericToEncoding (jsonOptions 1)

instance FromJSON Rational where
    parseJSON = Aeson.genericParseJSON (jsonOptions 1)

$(deriveTypeScript (jsonOptions 1) ''Rational)

instance Schema.ToSchema Rational where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 1))

instance ToJSON ConstantSumConfig where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON ConstantSumConfig where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''ConstantSumConfig)

instance Schema.ToSchema ConstantSumConfig where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))

instance ToJSON ActionEntry where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON ActionEntry where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''ActionEntry)

instance Schema.ToSchema ActionEntry where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSON ModuleState where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON ModuleState where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''ModuleState)

instance Schema.ToSchema ModuleState where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSON PoolDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON PoolDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''PoolDatum)

instance Schema.ToSchema PoolDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
