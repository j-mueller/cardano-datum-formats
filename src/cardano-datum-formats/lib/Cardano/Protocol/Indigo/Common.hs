{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Indigo.Common (
    Address (..),
    AssetClass (..),
    Credential (..),
    EpochToScaleKey (..),
    OnChainDecimal (..),
    OutputReference (..),
    SPInteger (..),
    StakeCredential (..),
    arbitraryAnyBuiltinByteString,
    arbitraryBuiltinByteString,
    fromMap,
    fromList,
    maybeFromOptionData,
    maybeToOptionData,
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions, stripFieldPrefix, sumOptions, sumOptionsWithFieldModifier)
import Cardano.Protocol.JSON ()
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Text qualified as T
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

newtype OnChainDecimal = OnChainDecimal
    { getOnChainInt :: Integer
    }
    deriving stock (Eq, Show, Generic)

data AssetClass = AssetClass
    { acCurrencySymbol :: PlutusTx.BuiltinByteString
    , acTokenName :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Ord, Show, Generic)

data OutputReference = OutputReference
    { orTransactionId :: PlutusTx.BuiltinByteString
    , orOutputIndex :: Integer
    }
    deriving stock (Eq, Ord, Show, Generic)

data Credential
    = PublicKeyCredential PlutusTx.BuiltinByteString
    | ScriptCredential PlutusTx.BuiltinByteString
    deriving stock (Eq, Ord, Show, Generic)

data StakeCredential
    = Inline Credential
    | Pointer
        { pointerSlotNumber :: Integer
        , pointerTransactionIndex :: Integer
        , pointerCertificateIndex :: Integer
        }
    deriving stock (Eq, Ord, Show, Generic)

data Address = Address
    { addressPaymentCredential :: Credential
    , addressStakeCredential :: Maybe StakeCredential
    }
    deriving stock (Eq, Ord, Show, Generic)

newtype SPInteger = SPInteger
    { spiValue :: Integer
    }
    deriving stock (Eq, Ord, Show, Generic)

data EpochToScaleKey = EpochToScaleKey
    { eskEpoch :: Integer
    , eskScale :: Integer
    }
    deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary OnChainDecimal where
    arbitrary = OnChainDecimal <$> arbitrary

instance Arbitrary AssetClass where
    arbitrary = AssetClass <$> arbitraryAnyBuiltinByteString <*> arbitraryAnyBuiltinByteString

instance Arbitrary OutputReference where
    arbitrary = OutputReference <$> arbitraryBuiltinByteString 32 <*> arbitrary

instance Arbitrary Credential where
    arbitrary =
        Gen.oneof
            [ PublicKeyCredential <$> arbitraryBuiltinByteString 28
            , ScriptCredential <$> arbitraryBuiltinByteString 28
            ]

instance Arbitrary StakeCredential where
    arbitrary =
        Gen.oneof
            [ Inline <$> arbitrary
            , Pointer <$> arbitrary <*> arbitrary <*> arbitrary
            ]

instance Arbitrary Address where
    arbitrary = Address <$> arbitrary <*> arbitrary

instance Arbitrary SPInteger where
    arbitrary = SPInteger <$> arbitrary

instance Arbitrary EpochToScaleKey where
    arbitrary = EpochToScaleKey <$> arbitrary <*> arbitrary

instance PTx.ToData OnChainDecimal where
    toBuiltinData (OnChainDecimal value) = PlutusTx.mkConstr 0 [PlutusTx.mkI value]

instance PTx.FromData OnChainDecimal where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just getOnChainInt]) -> Just OnChainDecimal{getOnChainInt}
        _ -> Nothing

instance PTx.ToData AssetClass where
    toBuiltinData AssetClass{acCurrencySymbol, acTokenName} =
        PlutusTx.mkConstr 0 [PlutusTx.mkB acCurrencySymbol, PlutusTx.mkB acTokenName]

instance PTx.FromData AssetClass where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just acCurrencySymbol, D.getB -> Just acTokenName]) ->
            Just AssetClass{acCurrencySymbol, acTokenName}
        _ -> Nothing

instance PTx.ToData OutputReference where
    toBuiltinData OutputReference{orTransactionId, orOutputIndex} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkConstr 0 [PlutusTx.mkB orTransactionId]
            , PlutusTx.mkI orOutputIndex
            ]

instance PTx.FromData OutputReference where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [txHash, D.getI -> Just orOutputIndex]) ->
            D.withConstr txHash $ \case
                (0, [D.getB -> Just orTransactionId]) ->
                    Just OutputReference{orTransactionId, orOutputIndex}
                _ -> Nothing
        _ -> Nothing

instance PTx.ToData Credential where
    toBuiltinData = \case
        PublicKeyCredential hashBytes ->
            PlutusTx.mkConstr 0 [PlutusTx.mkB hashBytes]
        ScriptCredential hashBytes ->
            PlutusTx.mkConstr 1 [PlutusTx.mkB hashBytes]

instance PTx.FromData Credential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just hashValue]) ->
            pure (PublicKeyCredential hashValue)
        (1, [D.getB -> Just hashValue]) ->
            pure (ScriptCredential hashValue)
        _ -> Nothing

instance PTx.ToData StakeCredential where
    toBuiltinData = \case
        Inline credential ->
            PlutusTx.mkConstr 0 [PTx.toBuiltinData credential]
        Pointer pointerSlotNumber pointerTransactionIndex pointerCertificateIndex ->
            PlutusTx.mkConstr
                1
                [ PlutusTx.mkConstr
                    0
                    [ PlutusTx.mkI pointerSlotNumber
                    , PlutusTx.mkI pointerTransactionIndex
                    , PlutusTx.mkI pointerCertificateIndex
                    ]
                ]

instance PTx.FromData StakeCredential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [credentialData]) ->
            Inline <$> PTx.fromBuiltinData credentialData
        (1, [pointerData]) -> do
            D.withConstr pointerData $ \case
                (0, [D.getI -> Just pointerSlotNumber, D.getI -> Just pointerTransactionIndex, D.getI -> Just pointerCertificateIndex]) ->
                    Just Pointer{pointerSlotNumber, pointerTransactionIndex, pointerCertificateIndex}
                _ -> Nothing
        _ -> Nothing

instance PTx.ToData Address where
    toBuiltinData Address{addressPaymentCredential, addressStakeCredential} =
        PlutusTx.mkConstr
            0
            [ PTx.toBuiltinData addressPaymentCredential
            , maybeToOptionData addressStakeCredential
            ]

instance PTx.FromData Address where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [paymentCredential, stakeCredential]) ->
            Address
                <$> PTx.fromBuiltinData paymentCredential
                <*> maybeFromOptionData PTx.fromBuiltinData stakeCredential
        _ -> Nothing

instance PTx.ToData SPInteger where
    toBuiltinData (SPInteger value) = PlutusTx.mkConstr 0 [PlutusTx.mkI value]

instance PTx.FromData SPInteger where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just spiValue]) -> Just SPInteger{spiValue}
        _ -> Nothing

instance PTx.ToData EpochToScaleKey where
    toBuiltinData EpochToScaleKey{eskEpoch, eskScale} =
        PlutusTx.mkConstr 0 [PlutusTx.mkI eskEpoch, PlutusTx.mkI eskScale]

instance PTx.FromData EpochToScaleKey where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just eskEpoch, D.getI -> Just eskScale]) ->
            Just EpochToScaleKey{eskEpoch, eskScale}
        _ -> Nothing

arbitraryBuiltinByteString :: Int -> QC.Gen PlutusTx.BuiltinByteString
arbitraryBuiltinByteString size =
    PlutusTx.toBuiltin . BS.pack <$> Gen.vectorOf size arbitrary

arbitraryAnyBuiltinByteString :: QC.Gen PlutusTx.BuiltinByteString
arbitraryAnyBuiltinByteString = do
    size <- Gen.chooseInt (0, 64)
    arbitraryBuiltinByteString size

fromList :: PlutusTx.BuiltinData -> Maybe [PlutusTx.BuiltinData]
fromList dt =
    PlutusTx.matchData
        dt
        (\_ _ -> Nothing)
        (const Nothing)
        Just
        (const Nothing)
        (const Nothing)

fromMap :: PlutusTx.BuiltinData -> Maybe [(PlutusTx.BuiltinData, PlutusTx.BuiltinData)]
fromMap dt =
    PlutusTx.matchData
        dt
        (\_ _ -> Nothing)
        Just
        (const Nothing)
        (const Nothing)
        (const Nothing)

maybeToOptionData :: PTx.ToData a => Maybe a -> PlutusTx.BuiltinData
maybeToOptionData = \case
    Nothing -> PlutusTx.mkConstr 1 []
    Just value -> PlutusTx.mkConstr 0 [PTx.toBuiltinData value]

maybeFromOptionData :: (PlutusTx.BuiltinData -> Maybe a) -> PlutusTx.BuiltinData -> Maybe (Maybe a)
maybeFromOptionData decode dt = D.withConstr dt $ \case
    (0, [value]) -> Just <$> decode value
    (1, []) -> Just Nothing
    _ -> Nothing

instance ToJSON OnChainDecimal where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON OnChainDecimal where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''OnChainDecimal)

instance Schema.ToSchema OnChainDecimal where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))

instance ToJSON AssetClass where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON AssetClass where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''AssetClass)

instance Schema.ToSchema AssetClass where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSON OutputReference where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON OutputReference where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''OutputReference)

instance Schema.ToSchema OutputReference where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSON Credential where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON Credential where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

$(deriveTypeScript (sumOptions 0) ''Credential)

instance Schema.ToSchema Credential where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

instance ToJSON StakeCredential where
    toJSON = Aeson.genericToJSON (sumOptionsWithFieldModifier 0 (stripFieldPrefix "pointer"))
    toEncoding = Aeson.genericToEncoding (sumOptionsWithFieldModifier 0 (stripFieldPrefix "pointer"))

instance FromJSON StakeCredential where
    parseJSON = Aeson.genericParseJSON (sumOptionsWithFieldModifier 0 (stripFieldPrefix "pointer"))

$(deriveTypeScript (sumOptionsWithFieldModifier 0 (stripFieldPrefix "pointer")) ''StakeCredential)

instance Schema.ToSchema StakeCredential where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptionsWithFieldModifier 0 (stripFieldPrefix "pointer")))

instance ToJSON Address where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance FromJSON Address where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

$(deriveTypeScript Aeson.defaultOptions ''Address)

instance Schema.ToSchema Address where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions Aeson.defaultOptions)

instance ToJSON SPInteger where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON SPInteger where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''SPInteger)

instance Schema.ToSchema SPInteger where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))

instance ToJSON EpochToScaleKey where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON EpochToScaleKey where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''EpochToScaleKey)

instance Schema.ToSchema EpochToScaleKey where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))

instance ToJSONKey EpochToScaleKey where
    toJSONKey = Aeson.toJSONKeyText $ \EpochToScaleKey{eskEpoch, eskScale} ->
        T.pack (show eskEpoch) <> ":" <> T.pack (show eskScale)

instance FromJSONKey EpochToScaleKey where
    fromJSONKey = Aeson.FromJSONKeyTextParser $ \text ->
        case T.breakOn ":" text of
            (epochText, scaleText)
                | T.null scaleText -> fail "unsupported EpochToScaleKey format"
                | otherwise ->
                    EpochToScaleKey
                        <$> parseJSONKey epochText
                        <*> parseJSONKey (T.drop 1 scaleText)
      where
        parseJSONKey :: FromJSON a => T.Text -> Aeson.Parser a
        parseJSONKey = Aeson.parseJSON . Aeson.String
