{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.VyFinance.Pool (
    PoolDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data PoolDatum = PoolDatum
    { pdAssetAQuantity :: Integer
    , pdAssetBQuantity :: Integer
    , pdLpQuantity :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary PoolDatum where
    arbitrary =
        PoolDatum
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary

instance PTx.ToData PoolDatum where
    toBuiltinData PoolDatum{pdAssetAQuantity, pdAssetBQuantity, pdLpQuantity} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkI pdAssetAQuantity
            , PlutusTx.mkI pdAssetBQuantity
            , PlutusTx.mkI pdLpQuantity
            ]

instance PTx.FromData PoolDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just pdAssetAQuantity, D.getI -> Just pdAssetBQuantity, D.getI -> Just pdLpQuantity]) ->
            pure PoolDatum{pdAssetAQuantity, pdAssetBQuantity, pdLpQuantity}
        _ -> Nothing

instance ToJSON PoolDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON PoolDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''PoolDatum)

instance Schema.ToSchema PoolDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
