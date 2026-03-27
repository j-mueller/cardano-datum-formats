{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Indigo.InterestOracle (
    InterestOracleDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.Indigo.Common (OnChainDecimal)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data InterestOracleDatum = InterestOracleDatum
    { iodUnitaryInterest :: Integer
    , iodInterestRate :: OnChainDecimal
    , iodLastUpdated :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary InterestOracleDatum where
    arbitrary = InterestOracleDatum <$> arbitrary <*> arbitrary <*> arbitrary

instance PTx.ToData InterestOracleDatum where
    toBuiltinData InterestOracleDatum{iodUnitaryInterest, iodInterestRate, iodLastUpdated} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkI iodUnitaryInterest
            , PTx.toBuiltinData iodInterestRate
            , PlutusTx.mkI iodLastUpdated
            ]

instance PTx.FromData InterestOracleDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just iodUnitaryInterest, interestRate, D.getI -> Just iodLastUpdated]) ->
            InterestOracleDatum
                <$> pure iodUnitaryInterest
                <*> PTx.fromBuiltinData interestRate
                <*> pure iodLastUpdated
        _ -> Nothing

instance ToJSON InterestOracleDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON InterestOracleDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''InterestOracleDatum)

instance Schema.ToSchema InterestOracleDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))
