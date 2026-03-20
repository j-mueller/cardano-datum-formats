module Cardano.Protocol.Liqwid.Action (
    ActionDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.Liqwid.Common (BatchState, fromList)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data ActionDatum = ActionDatum
    { adBatchState :: BatchState
    , adReservedSupply :: Integer
    }
    deriving stock (Eq, Show)

instance Arbitrary ActionDatum where
    arbitrary = ActionDatum <$> arbitrary <*> arbitrary

instance PTx.ToData ActionDatum where
    toBuiltinData ActionDatum{adBatchState, adReservedSupply} =
        PlutusTx.mkList [PTx.toBuiltinData adBatchState, PlutusTx.mkI adReservedSupply]

instance PTx.FromData ActionDatum where
    fromBuiltinData dt = do
        [batchState, D.getI -> Just adReservedSupply] <- fromList dt
        adBatchState <- PTx.fromBuiltinData batchState
        pure ActionDatum{adBatchState, adReservedSupply}
