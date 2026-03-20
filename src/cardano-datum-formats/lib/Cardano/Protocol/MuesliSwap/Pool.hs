module Cardano.Protocol.MuesliSwap.Pool (
    PoolDatum (..),
) where

import Cardano.Asset (Asset)
import Cardano.Data qualified as D
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data PoolDatum = PoolDatum
    { pdCoinA :: Asset
    , pdCoinB :: Asset
    , pdTotalLiquidity :: Integer
    , pdSwapFee :: Integer
    }
    deriving stock (Eq, Show)

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
