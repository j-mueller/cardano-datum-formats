{-# LANGUAGE TemplateHaskell #-}

module Cardano.Address.Aiken (
    Credential (..),
    StakeCredential (..),
    AikenAddress (..),
) where

import Cardano.Api qualified as C
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (
    jsonOptions,
    stripFieldPrefix,
    sumOptions,
    sumOptionsWithFieldModifier,
 )
import Cardano.Protocol.JSON ()
import Control.Monad ((>=>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Hedgehog qualified as H

data Credential
    = VerificationKeyCredential (C.Hash C.PaymentKey)
    | ScriptCredential C.ScriptHash
    deriving stock (Eq, Show, Generic)

instance Arbitrary Credential where
    arbitrary =
        Gen.oneof
            [ VerificationKeyCredential <$> H.hedgehog (Gen.genVerificationKeyHash C.AsPaymentKey)
            , ScriptCredential <$> H.hedgehog Gen.genScriptHash
            ]

instance PlutusTx.ToData Credential where
    toBuiltinData = \case
        VerificationKeyCredential keyHash -> PlutusTx.mkConstr 0 [D.serialiseHash keyHash]
        ScriptCredential scriptHash -> PlutusTx.mkConstr 1 [D.serialiseScriptHash scriptHash]

instance PlutusTx.FromData Credential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB >=> D.deserialiseHash -> Just keyHash]) -> Just (VerificationKeyCredential keyHash)
        (1, [D.getB >=> D.deserialiseScriptHash -> Just scriptHash]) -> Just (ScriptCredential scriptHash)
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
        Pointer pointerSlotNumber pointerTransactionIndex pointerCertificateIndex ->
            PlutusTx.mkConstr
                1
                [ PlutusTx.mkI pointerSlotNumber
                , PlutusTx.mkI pointerTransactionIndex
                , PlutusTx.mkI pointerCertificateIndex
                ]

instance PlutusTx.FromData StakeCredential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [credential]) -> Inline <$> PlutusTx.fromBuiltinData credential
        (1, [D.getI -> Just pointerSlotNumber, D.getI -> Just pointerTransactionIndex, D.getI -> Just pointerCertificateIndex]) ->
            Just Pointer{pointerSlotNumber, pointerTransactionIndex, pointerCertificateIndex}
        _ -> Nothing

data AikenAddress = AikenAddress
    { aaPaymentCredential :: Credential
    , aaStakeCredential :: Maybe StakeCredential
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary AikenAddress where
    arbitrary =
        AikenAddress <$> arbitrary <*> arbitrary

instance PlutusTx.ToData AikenAddress where
    toBuiltinData AikenAddress{aaPaymentCredential, aaStakeCredential} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.toBuiltinData aaPaymentCredential
            , maybeToOption aaStakeCredential
            ]

instance PlutusTx.FromData AikenAddress where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [paymentCredential, stakeCredential]) ->
            AikenAddress
                <$> PlutusTx.fromBuiltinData paymentCredential
                <*> fromOption PlutusTx.fromBuiltinData stakeCredential
        _ -> Nothing

maybeToOption :: PlutusTx.ToData a => Maybe a -> PlutusTx.BuiltinData
maybeToOption = \case
    Nothing -> PlutusTx.mkConstr 1 []
    Just value -> PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData value]

fromOption :: (PlutusTx.BuiltinData -> Maybe a) -> PlutusTx.BuiltinData -> Maybe (Maybe a)
fromOption decode dt = D.withConstr dt $ \case
    (0, [value]) -> Just <$> decode value
    (1, []) -> Just Nothing
    _ -> Nothing

instance ToJSON Credential where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON Credential where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

$(deriveTypeScript (sumOptions 0) ''Credential)

instance Schema.ToSchema Credential where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

stakeCredentialOptions :: Aeson.Options
stakeCredentialOptions =
    sumOptionsWithFieldModifier 0 (stripFieldPrefix "pointer")

instance ToJSON StakeCredential where
    toJSON = Aeson.genericToJSON stakeCredentialOptions
    toEncoding = Aeson.genericToEncoding stakeCredentialOptions

instance FromJSON StakeCredential where
    parseJSON = Aeson.genericParseJSON stakeCredentialOptions

$(deriveTypeScript (sumOptionsWithFieldModifier 0 (stripFieldPrefix "pointer")) ''StakeCredential)

instance Schema.ToSchema StakeCredential where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions stakeCredentialOptions)

instance ToJSON AikenAddress where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON AikenAddress where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''AikenAddress)

instance Schema.ToSchema AikenAddress where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
