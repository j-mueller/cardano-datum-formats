module Cardano.Protocol.VyFinance.Pool (
    PoolDatum (..),
) where

import Cardano.Data qualified as D
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data PoolDatum = PoolDatum
    { pdAssetAQuantity :: Integer
    , pdAssetBQuantity :: Integer
    , pdLpQuantity :: Integer
    }
    deriving stock (Eq, Show)

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
