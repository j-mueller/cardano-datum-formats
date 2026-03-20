module Cardano.Protocol.Pulse.SYVault (
    SYVaultDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Transaction.OutputReference (OutputReference)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data SYVaultDatum = SYVaultDatum
    { svdMarketId :: OutputReference
    , svdTotalSy :: Integer
    }
    deriving stock (Eq, Show)

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
