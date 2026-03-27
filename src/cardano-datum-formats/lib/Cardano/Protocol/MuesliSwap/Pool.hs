{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.MuesliSwap.Pool (
    PoolDatum (..),
) where

import Cardano.Asset (Asset)
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.JSON ()
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
    { pdCoinA :: Asset
    , pdCoinB :: Asset
    , pdTotalLiquidity :: Integer
    , pdSwapFee :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary PoolDatum where
    arbitrary =
        PoolDatum
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance PTx.ToData PoolDatum where
    toBuiltinData PoolDatum{pdCoinA, pdCoinB, pdTotalLiquidity, pdSwapFee} =
        PlutusTx.mkConstr
            0
            [ PTx.toBuiltinData pdCoinA
            , PTx.toBuiltinData pdCoinB
            , PlutusTx.mkI pdTotalLiquidity
            , PlutusTx.mkI pdSwapFee
            ]

instance PTx.FromData PoolDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [coinA, coinB, D.getI -> Just pdTotalLiquidity, D.getI -> Just pdSwapFee]) ->
            PoolDatum
                <$> PTx.fromBuiltinData coinA
                <*> PTx.fromBuiltinData coinB
                <*> pure pdTotalLiquidity
                <*> pure pdSwapFee
        _ -> Nothing

instance ToJSON PoolDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON PoolDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''PoolDatum)

instance Schema.ToSchema PoolDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
