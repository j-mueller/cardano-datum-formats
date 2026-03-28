{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Liqwid.Common (
    BatchState (..),
    IntegerPair (..),
    PubKeyHash (..),
    arbitraryBuiltinByteString,
    fromList,
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.JSON ()
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

newtype PubKeyHash = PubKeyHash
    { getPubKeyHash :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Show, Generic)

data BatchState = BatchState
    { bsField0 :: Integer
    , bsField1 :: Integer
    , bsField2 :: Integer
    , bsField3 :: Integer
    , bsField4 :: Integer
    }
    deriving stock (Eq, Show, Generic)

data IntegerPair = IntegerPair
    { ipField0 :: Integer
    , ipField1 :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary PubKeyHash where
    arbitrary = PubKeyHash <$> arbitraryBuiltinByteString 28

instance Arbitrary BatchState where
    arbitrary = BatchState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary IntegerPair where
    arbitrary = IntegerPair <$> arbitrary <*> arbitrary

instance PTx.ToData PubKeyHash where
    toBuiltinData (PubKeyHash bytes) = PlutusTx.mkB bytes

instance PTx.FromData PubKeyHash where
    fromBuiltinData dt = PubKeyHash <$> D.getB dt

instance PTx.ToData BatchState where
    toBuiltinData BatchState{bsField0, bsField1, bsField2, bsField3, bsField4} =
        PlutusTx.mkList
            [ PlutusTx.mkI bsField0
            , PlutusTx.mkI bsField1
            , PlutusTx.mkI bsField2
            , PlutusTx.mkI bsField3
            , PlutusTx.mkI bsField4
            ]

instance PTx.FromData BatchState where
    fromBuiltinData dt = do
        [D.getI -> Just bsField0, D.getI -> Just bsField1, D.getI -> Just bsField2, D.getI -> Just bsField3, D.getI -> Just bsField4] <- fromList dt
        pure BatchState{bsField0, bsField1, bsField2, bsField3, bsField4}

instance PTx.ToData IntegerPair where
    toBuiltinData IntegerPair{ipField0, ipField1} =
        PlutusTx.mkList [PlutusTx.mkI ipField0, PlutusTx.mkI ipField1]

instance PTx.FromData IntegerPair where
    fromBuiltinData dt = do
        [D.getI -> Just ipField0, D.getI -> Just ipField1] <- fromList dt
        pure IntegerPair{ipField0, ipField1}

arbitraryBuiltinByteString :: Int -> Gen.Gen PlutusTx.BuiltinByteString
arbitraryBuiltinByteString size =
    PlutusTx.toBuiltin . BS.pack <$> Gen.vectorOf size arbitrary

fromList :: PlutusTx.BuiltinData -> Maybe [PlutusTx.BuiltinData]
fromList dt =
    PlutusTx.matchData
        dt
        (\_ _ -> Nothing)
        (const Nothing)
        Just
        (const Nothing)
        (const Nothing)

instance ToJSON PubKeyHash where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON PubKeyHash where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''PubKeyHash)

instance Schema.ToSchema PubKeyHash where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))

instance ToJSON BatchState where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON BatchState where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''BatchState)

instance Schema.ToSchema BatchState where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSON IntegerPair where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON IntegerPair where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''IntegerPair)

instance Schema.ToSchema IntegerPair where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
