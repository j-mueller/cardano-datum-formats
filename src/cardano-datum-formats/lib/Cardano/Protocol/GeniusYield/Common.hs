{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.GeniusYield.Common (
    Address (..),
    AssetClass (..),
    Credential (..),
    OutputReference (..),
    RationalD (..),
    StakeCredential (..),
    Value (..),
    arbitraryAnyBuiltinByteString,
    arbitraryBuiltinByteString,
    fromMap,
    maybeFromOptionData,
    maybeToOptionData,
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions, stripFieldPrefix, sumOptions, sumOptionsWithFieldModifier)
import Cardano.Protocol.JSON ()
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

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

data RationalD = RationalD
    { rNumerator :: Integer
    , rDenominator :: Integer
    }
    deriving stock (Eq, Ord, Show, Generic)

data AssetClass = AssetClass
    { acSymbol :: PlutusTx.BuiltinByteString
    , acName :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Ord, Show, Generic)

newtype Value = Value
    { getValue :: Map PlutusTx.BuiltinByteString (Map PlutusTx.BuiltinByteString Integer)
    }
    deriving stock (Eq, Show, Generic)

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

instance Arbitrary RationalD where
    arbitrary = RationalD <$> arbitrary <*> arbitrary

instance Arbitrary AssetClass where
    arbitrary = AssetClass <$> arbitraryAnyBuiltinByteString <*> arbitraryAnyBuiltinByteString

instance Arbitrary Value where
    arbitrary = do
        policies <- Gen.listOf ((,) <$> arbitraryAnyBuiltinByteString <*> Gen.listOf ((,) <$> arbitraryAnyBuiltinByteString <*> arbitrary))
        pure $ Value $ Map.fromList [(policy, Map.fromList assets) | (policy, assets) <- policies]

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
        PublicKeyCredential hashBytes -> PlutusTx.mkConstr 0 [PlutusTx.mkB hashBytes]
        ScriptCredential hashBytes -> PlutusTx.mkConstr 1 [PlutusTx.mkB hashBytes]

instance PTx.FromData Credential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just hashValue]) -> pure (PublicKeyCredential hashValue)
        (1, [D.getB -> Just hashValue]) -> pure (ScriptCredential hashValue)
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
        (1, [pointerData]) ->
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

instance PTx.ToData RationalD where
    toBuiltinData RationalD{rNumerator, rDenominator} =
        PlutusTx.mkConstr 0 [PlutusTx.mkI rNumerator, PlutusTx.mkI rDenominator]

instance PTx.FromData RationalD where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just rNumerator, D.getI -> Just rDenominator]) ->
            Just RationalD{rNumerator, rDenominator}
        _ -> Nothing

instance PTx.ToData AssetClass where
    toBuiltinData AssetClass{acSymbol, acName} =
        PlutusTx.mkConstr 0 [PlutusTx.mkB acSymbol, PlutusTx.mkB acName]

instance PTx.FromData AssetClass where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just acSymbol, D.getB -> Just acName]) ->
            Just AssetClass{acSymbol, acName}
        _ -> Nothing

instance PTx.ToData Value where
    toBuiltinData (Value valueMap) =
        PlutusTx.mkMap
            [ ( PlutusTx.mkB policyId
              , PlutusTx.mkMap
                    [ (PlutusTx.mkB assetName, PlutusTx.mkI quantity)
                    | (assetName, quantity) <- Map.toAscList assets
                    ]
              )
            | (policyId, assets) <- Map.toAscList valueMap
            ]

instance PTx.FromData Value where
    fromBuiltinData dt = do
        policyEntries <- fromMap dt
        valueMap <-
            traverse
                ( \(policyData, assetsData) -> do
                    policyId <- D.getB policyData
                    assetEntries <- fromMap assetsData
                    assets <-
                        traverse
                            (\(assetNameData, quantityData) -> do
                                assetName <- D.getB assetNameData
                                quantity <- D.getI quantityData
                                pure (assetName, quantity))
                            assetEntries
                    pure (policyId, Map.fromList assets)
                )
                policyEntries
        pure $ Value (Map.fromList valueMap)

arbitraryBuiltinByteString :: Int -> QC.Gen PlutusTx.BuiltinByteString
arbitraryBuiltinByteString size =
    PlutusTx.toBuiltin . BS.pack <$> Gen.vectorOf size arbitrary

arbitraryAnyBuiltinByteString :: QC.Gen PlutusTx.BuiltinByteString
arbitraryAnyBuiltinByteString = do
    size <- Gen.chooseInt (0, 64)
    arbitraryBuiltinByteString size

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

instance ToJSON OutputReference where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON OutputReference where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''OutputReference)

instance Schema.ToSchema OutputReference where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSONKey OutputReference where
    toJSONKey =
        Aeson.toJSONKeyText $ \OutputReference{orTransactionId, orOutputIndex} ->
            TE.decodeUtf8 (Base16.encode (PlutusTx.fromBuiltin orTransactionId)) <> ":" <> T.pack (show orOutputIndex)

instance FromJSONKey OutputReference where
    fromJSONKey = Aeson.FromJSONKeyTextParser $ \text ->
        case T.breakOn ":" text of
            (txIdText, outputIndexText)
                | T.null outputIndexText -> fail "unsupported OutputReference key format"
                | otherwise ->
                    OutputReference
                        <$> parseJSONKey txIdText
                        <*> parseJSONKey (T.drop 1 outputIndexText)
      where
        parseJSONKey :: FromJSON a => T.Text -> Aeson.Parser a
        parseJSONKey = Aeson.parseJSON . Aeson.String

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

instance ToJSON RationalD where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance FromJSON RationalD where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

$(deriveTypeScript Aeson.defaultOptions ''RationalD)

instance Schema.ToSchema RationalD where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions Aeson.defaultOptions)

instance ToJSON AssetClass where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance FromJSON AssetClass where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

$(deriveTypeScript Aeson.defaultOptions ''AssetClass)

instance Schema.ToSchema AssetClass where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions Aeson.defaultOptions)

instance ToJSON Value where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance FromJSON Value where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

$(deriveTypeScript Aeson.defaultOptions ''Value)

instance Schema.ToSchema Value where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions Aeson.defaultOptions)
