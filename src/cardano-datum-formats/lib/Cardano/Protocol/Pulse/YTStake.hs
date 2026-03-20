module Cardano.Protocol.Pulse.YTStake (
    YTStakeDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.Pulse.Common (PubKeyHash)
import Cardano.Transaction.OutputReference (OutputReference)
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
    deriving stock (Eq, Show)

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
