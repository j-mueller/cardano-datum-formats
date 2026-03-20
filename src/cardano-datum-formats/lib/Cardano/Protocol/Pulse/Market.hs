module Cardano.Protocol.Pulse.Market (
    MarketDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Transaction.OutputReference (OutputReference)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data MarketDatum = MarketDatum
    { mdMarketId :: OutputReference
    , mdTotalSy :: Integer
    , mdTotalPt :: Integer
    , mdTotalLp :: Integer
    , mdReservedLp :: Integer
    }
    deriving stock (Eq, Show)

instance Arbitrary MarketDatum where
    arbitrary =
        MarketDatum
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance PTx.ToData MarketDatum where
    toBuiltinData MarketDatum{mdMarketId, mdTotalSy, mdTotalPt, mdTotalLp, mdReservedLp} =
        PlutusTx.mkConstr
            0
            [ PTx.toBuiltinData mdMarketId
            , PlutusTx.mkI mdTotalSy
            , PlutusTx.mkI mdTotalPt
            , PlutusTx.mkI mdTotalLp
            , PlutusTx.mkI mdReservedLp
            ]

instance PTx.FromData MarketDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [marketId, D.getI -> Just mdTotalSy, D.getI -> Just mdTotalPt, D.getI -> Just mdTotalLp, D.getI -> Just mdReservedLp]) ->
            MarketDatum
                <$> PTx.fromBuiltinData marketId
                <*> pure mdTotalSy
                <*> pure mdTotalPt
                <*> pure mdTotalLp
                <*> pure mdReservedLp
        _ -> Nothing
