{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Pulse.Common (
    PubKeyHash (..),
    maybeToOptionData,
    maybeFromOptionData,
    maybeToOptionRawData,
    maybeFromOptionRawData,
) where

import Cardano.Api qualified as C
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.JSON ()
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
import Test.QuickCheck.Hedgehog qualified as H

newtype PubKeyHash = PubKeyHash
    { getPubKeyHash :: C.Hash C.PaymentKey
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary PubKeyHash where
    arbitrary =
        PubKeyHash <$> H.hedgehog (Gen.genVerificationKeyHash C.AsPaymentKey)

instance PlutusTx.ToData PubKeyHash where
    toBuiltinData (PubKeyHash keyHash) = D.serialiseHash keyHash

instance PlutusTx.FromData PubKeyHash where
    fromBuiltinData dt =
        PubKeyHash <$> (D.getB dt >>= D.deserialiseHash)

maybeToOptionData :: PlutusTx.ToData a => Maybe a -> PlutusTx.BuiltinData
maybeToOptionData = \case
    Nothing -> PlutusTx.mkConstr 1 []
    Just value -> PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData value]

maybeFromOptionData :: (PlutusTx.BuiltinData -> Maybe a) -> PlutusTx.BuiltinData -> Maybe (Maybe a)
maybeFromOptionData decode dt = D.withConstr dt $ \case
    (0, [value]) -> Just <$> decode value
    (1, []) -> Just Nothing
    _ -> Nothing

maybeToOptionRawData :: Maybe PlutusTx.BuiltinData -> PlutusTx.BuiltinData
maybeToOptionRawData = \case
    Nothing -> PlutusTx.mkConstr 1 []
    Just value -> PlutusTx.mkConstr 0 [value]

maybeFromOptionRawData :: PlutusTx.BuiltinData -> Maybe (Maybe PlutusTx.BuiltinData)
maybeFromOptionRawData dt = D.withConstr dt $ \case
    (0, [value]) -> Just (Just value)
    (1, []) -> Just Nothing
    _ -> Nothing

instance ToJSON PubKeyHash where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON PubKeyHash where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''PubKeyHash)

instance Schema.ToSchema PubKeyHash where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))
