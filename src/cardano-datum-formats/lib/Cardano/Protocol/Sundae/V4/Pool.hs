{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Sundae.V4.Pool (
    AssetAmount (..),
    ActionEntry (..),
    ModuleState (..),
    PoolDatum (..),
) where

import Cardano.Api qualified as C
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.Sundae.V4.Common
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

data ActionEntry = ActionEntry
    { aeTag :: Integer
    , aeEnabled :: Bool
    , aeModules :: [PlutusTx.BuiltinByteString]
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary ActionEntry where
    arbitrary = ActionEntry <$> arbitrary <*> arbitrary <*> Gen.listOf arbitraryBuiltinByteString

instance PlutusTx.ToData ActionEntry where
    toBuiltinData ActionEntry{aeTag, aeEnabled, aeModules} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkI aeTag
            , boolToData aeEnabled
            , PlutusTx.toBuiltinData aeModules
            ]

instance PlutusTx.FromData ActionEntry where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just aeTag, enabled, modules]) ->
            ActionEntry aeTag <$> boolFromData enabled <*> PlutusTx.fromBuiltinData modules
        _ -> Nothing

data ModuleState = ModuleState
    { msModuleHash :: PlutusTx.BuiltinByteString
    , msState :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary ModuleState where
    arbitrary = ModuleState <$> arbitraryBuiltinByteString <*> arbitraryBuiltinByteString

instance PlutusTx.ToData ModuleState where
    toBuiltinData ModuleState{msModuleHash, msState} =
        PlutusTx.mkList [PlutusTx.mkB msModuleHash, PlutusTx.mkB msState]

instance PlutusTx.FromData ModuleState where
    fromBuiltinData dt = fromList dt $ \case
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
            <*> arbitraryBuiltinByteString
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

boolToData :: Bool -> PlutusTx.BuiltinData
boolToData = \case
    False -> PlutusTx.mkConstr 0 []
    True -> PlutusTx.mkConstr 1 []

boolFromData :: PlutusTx.BuiltinData -> Maybe Bool
boolFromData dt = D.withConstr dt $ \case
    (0, []) -> Just False
    (1, []) -> Just True
    _ -> Nothing

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
