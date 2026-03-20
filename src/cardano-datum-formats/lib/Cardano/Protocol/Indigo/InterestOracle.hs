module Cardano.Protocol.Indigo.InterestOracle (
    InterestOracleDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.Indigo.Common (OnChainDecimal)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data InterestOracleDatum = InterestOracleDatum
    { iodUnitaryInterest :: Integer
    , iodInterestRate :: OnChainDecimal
    , iodLastUpdated :: Integer
    }
    deriving stock (Eq, Show)

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
