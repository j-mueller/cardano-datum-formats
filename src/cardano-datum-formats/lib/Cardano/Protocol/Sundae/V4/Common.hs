{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Sundae.V4.Common (
    MultisigScript (..),
    Credential (..),
    StakeCredential (..),
    SundaeAddress (..),
    AssetAmount (..),
    arbitraryBuiltinByteString,
    arbitraryBuiltinData,
    fromList,
    fromOption,
    toOption,
) where

import Cardano.Api qualified as C
import Cardano.Asset (Asset)
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions, stripFieldPrefix, sumOptions, sumOptionsWithFieldModifier)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.ByteString qualified as BS
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import Data.Word (Word8)
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data MultisigScript
    = Signature PlutusTx.BuiltinByteString
    | AllOf [MultisigScript]
    | AnyOf [MultisigScript]
    | AtLeast Integer [MultisigScript]
    | Before Integer
    | After Integer
    | Script PlutusTx.BuiltinByteString
    deriving stock (Eq, Show, Generic)

instance Arbitrary MultisigScript where
    arbitrary = QC.sized go
      where
        go n
            | n <= 1 =
                Gen.oneof
                    [ Signature <$> arbitraryBuiltinByteString
                    , Script <$> arbitraryBuiltinByteString
                    , Before <$> arbitrary
                    , After <$> arbitrary
                    ]
            | otherwise =
                Gen.frequency
                    [ (4, go 1)
                    , (1, AllOf <$> Gen.resize (n `div` 2) arbitrary)
                    , (1, AnyOf <$> Gen.resize (n `div` 2) arbitrary)
                    , (1, AtLeast <$> arbitrary <*> Gen.resize (n `div` 2) arbitrary)
                    ]

instance PlutusTx.ToData MultisigScript where
    toBuiltinData = \case
        Signature keyHash -> PlutusTx.mkConstr 0 [PlutusTx.mkB keyHash]
        AllOf scripts -> PlutusTx.mkConstr 1 [PlutusTx.toBuiltinData scripts]
        AnyOf scripts -> PlutusTx.mkConstr 2 [PlutusTx.toBuiltinData scripts]
        AtLeast required scripts -> PlutusTx.mkConstr 3 [PlutusTx.mkI required, PlutusTx.toBuiltinData scripts]
        Before time -> PlutusTx.mkConstr 4 [PlutusTx.mkI time]
        After time -> PlutusTx.mkConstr 5 [PlutusTx.mkI time]
        Script scriptHash -> PlutusTx.mkConstr 6 [PlutusTx.mkB scriptHash]

instance PlutusTx.FromData MultisigScript where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just keyHash]) -> Just (Signature keyHash)
        (1, [scripts]) -> AllOf <$> PlutusTx.fromBuiltinData scripts
        (2, [scripts]) -> AnyOf <$> PlutusTx.fromBuiltinData scripts
        (3, [D.getI -> Just required, scripts]) -> AtLeast required <$> PlutusTx.fromBuiltinData scripts
        (4, [D.getI -> Just time]) -> Just (Before time)
        (5, [D.getI -> Just time]) -> Just (After time)
        (6, [D.getB -> Just scriptHash]) -> Just (Script scriptHash)
        _ -> Nothing

data Credential
    = VerificationKeyCredential PlutusTx.BuiltinByteString
    | ScriptCredential PlutusTx.BuiltinByteString
    deriving stock (Eq, Show, Generic)

instance Arbitrary Credential where
    arbitrary =
        Gen.oneof
            [ VerificationKeyCredential <$> arbitraryBuiltinByteString
            , ScriptCredential <$> arbitraryBuiltinByteString
            ]

instance PlutusTx.ToData Credential where
    toBuiltinData = \case
        VerificationKeyCredential keyHash -> PlutusTx.mkConstr 0 [PlutusTx.mkB keyHash]
        ScriptCredential scriptHash -> PlutusTx.mkConstr 1 [PlutusTx.mkB scriptHash]

instance PlutusTx.FromData Credential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just keyHash]) -> Just (VerificationKeyCredential keyHash)
        (1, [D.getB -> Just scriptHash]) -> Just (ScriptCredential scriptHash)
        _ -> Nothing

data StakeCredential
    = Inline Credential
    | Pointer
        { pointerSlotNumber :: Integer
        , pointerTransactionIndex :: Integer
        , pointerCertificateIndex :: Integer
        }
    deriving stock (Eq, Show, Generic)

instance Arbitrary StakeCredential where
    arbitrary =
        Gen.oneof
            [ Inline <$> arbitrary
            , Pointer <$> arbitrary <*> arbitrary <*> arbitrary
            ]

instance PlutusTx.ToData StakeCredential where
    toBuiltinData = \case
        Inline credential -> PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData credential]
        Pointer slotNumber transactionIndex certificateIndex ->
            PlutusTx.mkConstr
                1
                [ PlutusTx.mkI slotNumber
                , PlutusTx.mkI transactionIndex
                , PlutusTx.mkI certificateIndex
                ]

instance PlutusTx.FromData StakeCredential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [credential]) -> Inline <$> PlutusTx.fromBuiltinData credential
        (1, [D.getI -> Just slotNumber, D.getI -> Just transactionIndex, D.getI -> Just certificateIndex]) ->
            Just (Pointer slotNumber transactionIndex certificateIndex)
        _ -> Nothing

data SundaeAddress = SundaeAddress
    { saPaymentCredential :: Credential
    , saStakeCredential :: Maybe StakeCredential
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary SundaeAddress where
    arbitrary = SundaeAddress <$> arbitrary <*> arbitrary

instance PlutusTx.ToData SundaeAddress where
    toBuiltinData SundaeAddress{saPaymentCredential, saStakeCredential} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.toBuiltinData saPaymentCredential
            , toOption PlutusTx.toBuiltinData saStakeCredential
            ]

instance PlutusTx.FromData SundaeAddress where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [paymentCredential, stakeCredential]) ->
            SundaeAddress
                <$> PlutusTx.fromBuiltinData paymentCredential
                <*> fromOption PlutusTx.fromBuiltinData stakeCredential
        _ -> Nothing

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
    fromBuiltinData dt = fromList dt $ \case
        [asset, D.getI -> Just amount] -> AssetAmount <$> PlutusTx.fromBuiltinData asset <*> pure (C.Quantity amount)
        _ -> Nothing

arbitraryBuiltinByteString :: QC.Gen PlutusTx.BuiltinByteString
arbitraryBuiltinByteString = PlutusTx.toBuiltin . BS.pack <$> (arbitrary :: QC.Gen [Word8])

arbitraryBuiltinData :: QC.Gen PlutusTx.BuiltinData
arbitraryBuiltinData =
    Gen.elements
        [ PlutusTx.mkConstr 0 []
        , PlutusTx.mkList []
        , PlutusTx.mkI 0
        , PlutusTx.mkB (PlutusTx.toBuiltin BS.empty)
        ]

fromList :: PlutusTx.BuiltinData -> ([PlutusTx.BuiltinData] -> Maybe a) -> Maybe a
fromList dt match =
    PlutusTx.matchData dt (\_ _ -> Nothing) (const Nothing) match (const Nothing) (const Nothing)

toOption :: (a -> PlutusTx.BuiltinData) -> Maybe a -> PlutusTx.BuiltinData
toOption encode = \case
    Just value -> PlutusTx.mkConstr 0 [encode value]
    Nothing -> PlutusTx.mkConstr 1 []

fromOption :: (PlutusTx.BuiltinData -> Maybe a) -> PlutusTx.BuiltinData -> Maybe (Maybe a)
fromOption decode dt = D.withConstr dt $ \case
    (0, [value]) -> Just <$> decode value
    (1, []) -> Just Nothing
    _ -> Nothing

instance ToJSON MultisigScript where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON MultisigScript where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

$(deriveTypeScript (sumOptions 0) ''MultisigScript)

instance Schema.ToSchema MultisigScript where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

instance ToJSON Credential where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON Credential where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

$(deriveTypeScript (sumOptions 0) ''Credential)

instance Schema.ToSchema Credential where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

stakeCredentialOptions :: Aeson.Options
stakeCredentialOptions = sumOptionsWithFieldModifier 0 (stripFieldPrefix "pointer")

instance ToJSON StakeCredential where
    toJSON = Aeson.genericToJSON stakeCredentialOptions
    toEncoding = Aeson.genericToEncoding stakeCredentialOptions

instance FromJSON StakeCredential where
    parseJSON = Aeson.genericParseJSON stakeCredentialOptions

$(deriveTypeScript (sumOptionsWithFieldModifier 0 (stripFieldPrefix "pointer")) ''StakeCredential)

instance Schema.ToSchema StakeCredential where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions stakeCredentialOptions)

instance ToJSON SundaeAddress where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON SundaeAddress where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''SundaeAddress)

instance Schema.ToSchema SundaeAddress where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSON AssetAmount where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON AssetAmount where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''AssetAmount)

instance Schema.ToSchema AssetAmount where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
