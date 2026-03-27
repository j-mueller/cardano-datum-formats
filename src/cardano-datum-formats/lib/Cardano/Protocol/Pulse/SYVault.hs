{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Pulse.SYVault (
    SYVaultDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.JSON ()
import Cardano.Transaction.OutputReference (OutputReference)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data SYVaultDatum = SYVaultDatum
    { svdMarketId :: OutputReference
    , svdTotalSy :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary SYVaultDatum where
    arbitrary =
        SYVaultDatum
            <$> arbitrary
            <*> arbitrary

instance PTx.ToData SYVaultDatum where
    toBuiltinData SYVaultDatum{svdMarketId, svdTotalSy} =
        PlutusTx.mkConstr
            0
            [ PTx.toBuiltinData svdMarketId
            , PlutusTx.mkI svdTotalSy
            ]

instance PTx.FromData SYVaultDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [marketId, D.getI -> Just svdTotalSy]) ->
            SYVaultDatum
                <$> PTx.fromBuiltinData marketId
                <*> pure svdTotalSy
        _ -> Nothing

instance ToJSON SYVaultDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON SYVaultDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''SYVaultDatum)

instance Schema.ToSchema SYVaultDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))
