{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Pulse.Oracle (
    OracleDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data OracleDatum = OracleDatum
    { odPyIndex :: Integer
    , odBased :: Integer
    , odUpdatedAt :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary OracleDatum where
    arbitrary =
        OracleDatum
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary

instance PTx.ToData OracleDatum where
    toBuiltinData OracleDatum{odPyIndex, odBased, odUpdatedAt} =
        PlutusTx.mkConstr 0 [PlutusTx.mkI odPyIndex, PlutusTx.mkI odBased, PlutusTx.mkI odUpdatedAt]

instance PTx.FromData OracleDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just odPyIndex, D.getI -> Just odBased, D.getI -> Just odUpdatedAt]) ->
            Just OracleDatum{odPyIndex, odBased, odUpdatedAt}
        _ -> Nothing

instance ToJSON OracleDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON OracleDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''OracleDatum)

instance Schema.ToSchema OracleDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
