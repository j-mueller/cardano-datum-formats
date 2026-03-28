{-# LANGUAGE TemplateHaskell #-}

module Cardano.Transaction.OutputReference (
    OutputReference (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON ()
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.ByteString qualified as BS
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data OutputReference = OutputReference
    { orTransactionId :: PlutusTx.BuiltinByteString
    , orOutputIndex :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary OutputReference where
    arbitrary =
        OutputReference
            <$> arbitraryByteString 32
            <*> arbitrary

instance PlutusTx.ToData OutputReference where
    toBuiltinData OutputReference{orTransactionId, orOutputIndex} =
        PlutusTx.mkConstr 0 [PlutusTx.mkB orTransactionId, PlutusTx.mkI orOutputIndex]

instance PlutusTx.FromData OutputReference where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just orTransactionId, D.getI -> Just orOutputIndex]) ->
            Just OutputReference{orTransactionId, orOutputIndex}
        _ -> Nothing

arbitraryByteString :: Int -> QC.Gen PlutusTx.BuiltinByteString
arbitraryByteString size =
    PlutusTx.toBuiltin . BS.pack <$> Gen.vectorOf size arbitrary

instance ToJSON OutputReference where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance FromJSON OutputReference where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

$(deriveTypeScript Aeson.defaultOptions ''OutputReference)

instance Schema.ToSchema OutputReference where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions Aeson.defaultOptions)
