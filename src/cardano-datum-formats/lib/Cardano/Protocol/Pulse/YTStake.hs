{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Pulse.YTStake (
    YTStakeDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.JSON ()
import Cardano.Protocol.Pulse.Common (PubKeyHash)
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

data YTStakeDatum = YTStakeDatum
    { ysdMarketId :: OutputReference
    , ysdStakeId :: OutputReference
    , ysdOwnerPkh :: PubKeyHash
    , ysdStakedYtAmount :: Integer
    , ysdUnclaimedSyAmount :: Integer
    , ysdPyIndex :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary YTStakeDatum where
    arbitrary =
        YTStakeDatum
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance PTx.ToData YTStakeDatum where
    toBuiltinData YTStakeDatum{ysdMarketId, ysdStakeId, ysdOwnerPkh, ysdStakedYtAmount, ysdUnclaimedSyAmount, ysdPyIndex} =
        PlutusTx.mkConstr
            0
            [ PTx.toBuiltinData ysdMarketId
            , PTx.toBuiltinData ysdStakeId
            , PTx.toBuiltinData ysdOwnerPkh
            , PlutusTx.mkI ysdStakedYtAmount
            , PlutusTx.mkI ysdUnclaimedSyAmount
            , PlutusTx.mkI ysdPyIndex
            ]

instance PTx.FromData YTStakeDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [marketId, stakeId, ownerPkh, D.getI -> Just ysdStakedYtAmount, D.getI -> Just ysdUnclaimedSyAmount, D.getI -> Just ysdPyIndex]) ->
            YTStakeDatum
                <$> PTx.fromBuiltinData marketId
                <*> PTx.fromBuiltinData stakeId
                <*> PTx.fromBuiltinData ownerPkh
                <*> pure ysdStakedYtAmount
                <*> pure ysdUnclaimedSyAmount
                <*> pure ysdPyIndex
        _ -> Nothing

instance ToJSON YTStakeDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON YTStakeDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''YTStakeDatum)

instance Schema.ToSchema YTStakeDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))
