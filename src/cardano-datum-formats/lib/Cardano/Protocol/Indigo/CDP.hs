{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Indigo.CDP (
    CDPDatum (..),
    CDPFees (..),
    CDPContent (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions, stripFieldPrefix, sumOptionsWithFieldModifier)
import Cardano.Protocol.Indigo.Common (arbitraryAnyBuiltinByteString, maybeFromOptionData, maybeToOptionData)
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

data CDPFees
    = ActiveCDPInterestTracking
        { cdpfLastSettled :: Integer
        , cdpfUnitaryInterestSnapshot :: Integer
        }
    | FrozenCDPAccumulatedFees
        { cdpfLovelacesTreasury :: Integer
        , cdpfLovelacesIndyStakers :: Integer
        }
    deriving stock (Eq, Show, Generic)

data CDPContent = CDPContent
    { ccCdpOwner :: Maybe PlutusTx.BuiltinByteString
    , ccIAsset :: PlutusTx.BuiltinByteString
    , ccMintedAmt :: Integer
    , ccCdpFees :: CDPFees
    }
    deriving stock (Eq, Show, Generic)

newtype CDPDatum = CDPDatum
    { getCDPContent :: CDPContent
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary CDPFees where
    arbitrary =
        Gen.oneof
            [ ActiveCDPInterestTracking <$> arbitrary <*> arbitrary
            , FrozenCDPAccumulatedFees <$> arbitrary <*> arbitrary
            ]

instance Arbitrary CDPContent where
    arbitrary =
        CDPContent
            <$> arbitraryMaybeBytes
            <*> arbitraryAnyBuiltinByteString
            <*> arbitrary
            <*> arbitrary

instance Arbitrary CDPDatum where
    arbitrary = CDPDatum <$> arbitrary

instance PTx.ToData CDPFees where
    toBuiltinData = \case
        ActiveCDPInterestTracking cdpfLastSettled cdpfUnitaryInterestSnapshot ->
            PlutusTx.mkConstr 0 [PlutusTx.mkI cdpfLastSettled, PlutusTx.mkI cdpfUnitaryInterestSnapshot]
        FrozenCDPAccumulatedFees cdpfLovelacesTreasury cdpfLovelacesIndyStakers ->
            PlutusTx.mkConstr 1 [PlutusTx.mkI cdpfLovelacesTreasury, PlutusTx.mkI cdpfLovelacesIndyStakers]

instance PTx.FromData CDPFees where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just cdpfLastSettled, D.getI -> Just cdpfUnitaryInterestSnapshot]) ->
            Just ActiveCDPInterestTracking{cdpfLastSettled, cdpfUnitaryInterestSnapshot}
        (1, [D.getI -> Just cdpfLovelacesTreasury, D.getI -> Just cdpfLovelacesIndyStakers]) ->
            Just FrozenCDPAccumulatedFees{cdpfLovelacesTreasury, cdpfLovelacesIndyStakers}
        _ -> Nothing

instance PTx.ToData CDPContent where
    toBuiltinData CDPContent{ccCdpOwner, ccIAsset, ccMintedAmt, ccCdpFees} =
        PlutusTx.mkConstr
            0
            [ maybeToOptionDataBytes ccCdpOwner
            , PlutusTx.mkB ccIAsset
            , PlutusTx.mkI ccMintedAmt
            , PTx.toBuiltinData ccCdpFees
            ]

instance PTx.FromData CDPContent where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [owner, D.getB -> Just ccIAsset, D.getI -> Just ccMintedAmt, cdpFees]) ->
            CDPContent
                <$> maybeFromOptionData D.getB owner
                <*> pure ccIAsset
                <*> pure ccMintedAmt
                <*> PTx.fromBuiltinData cdpFees
        _ -> Nothing

instance PTx.ToData CDPDatum where
    toBuiltinData (CDPDatum content) =
        PlutusTx.mkConstr 0 [cdpContentToVariantData content]

instance PTx.FromData CDPDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [content]) -> CDPDatum <$> cdpContentFromVariantData content
        _ -> Nothing

arbitraryMaybeBytes :: Gen.Gen (Maybe PlutusTx.BuiltinByteString)
arbitraryMaybeBytes = Gen.oneof [pure Nothing, Just <$> arbitraryAnyBuiltinByteString]

cdpContentToVariantData :: CDPContent -> PlutusTx.BuiltinData
cdpContentToVariantData CDPContent{ccCdpOwner, ccIAsset, ccMintedAmt, ccCdpFees} =
    PlutusTx.mkConstr
        0
        [ maybeToOptionDataBytes ccCdpOwner
        , PlutusTx.mkB ccIAsset
        , PlutusTx.mkI ccMintedAmt
        , PTx.toBuiltinData ccCdpFees
        ]

cdpContentFromVariantData :: PlutusTx.BuiltinData -> Maybe CDPContent
cdpContentFromVariantData dt = D.withConstr dt $ \case
    (0, [owner, D.getB -> Just ccIAsset, D.getI -> Just ccMintedAmt, cdpFees]) ->
        CDPContent
            <$> maybeFromOptionData D.getB owner
            <*> pure ccIAsset
            <*> pure ccMintedAmt
            <*> PTx.fromBuiltinData cdpFees
    _ -> Nothing

maybeToOptionDataBytes :: Maybe PlutusTx.BuiltinByteString -> PlutusTx.BuiltinData
maybeToOptionDataBytes = maybeToOptionData . fmap BytesAsData

newtype BytesAsData = BytesAsData { unBytesAsData :: PlutusTx.BuiltinByteString }

instance PTx.ToData BytesAsData where
    toBuiltinData = PlutusTx.mkB . unBytesAsData

instance ToJSON CDPFees where
    toJSON = Aeson.genericToJSON (sumOptionsWithFieldModifier 0 (stripFieldPrefix "cdpf"))
    toEncoding = Aeson.genericToEncoding (sumOptionsWithFieldModifier 0 (stripFieldPrefix "cdpf"))

instance FromJSON CDPFees where
    parseJSON = Aeson.genericParseJSON (sumOptionsWithFieldModifier 0 (stripFieldPrefix "cdpf"))

$(deriveTypeScript (sumOptionsWithFieldModifier 0 (stripFieldPrefix "cdpf")) ''CDPFees)

instance Schema.ToSchema CDPFees where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptionsWithFieldModifier 0 (stripFieldPrefix "cdpf")))

instance ToJSON CDPContent where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON CDPContent where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''CDPContent)

instance Schema.ToSchema CDPContent where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSON CDPDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON CDPDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''CDPDatum)

instance Schema.ToSchema CDPDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))
