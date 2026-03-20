module Cardano.Protocol.Pulse.Oracle (
    OracleDatum (..),
) where

import Cardano.Data qualified as D
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data OracleDatum = OracleDatum
    { odPyIndex :: Integer
    , odBased :: Integer
    , odUpdatedAt :: Integer
    }
    deriving stock (Eq, Show)

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
