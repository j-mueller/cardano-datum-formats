module Cardano.Protocol.Indigo.LRP (
    LRPDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.Indigo.Common (OnChainDecimal, arbitraryAnyBuiltinByteString)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data LRPDatum = LRPDatum
    { ldOwner :: PlutusTx.BuiltinByteString
    , ldIAsset :: PlutusTx.BuiltinByteString
    , ldMaxPrice :: OnChainDecimal
    , ldLovelacesToSpend :: Integer
    }
    deriving stock (Eq, Show)

instance Arbitrary LRPDatum where
    arbitrary = LRPDatum <$> arbitraryAnyBuiltinByteString <*> arbitraryAnyBuiltinByteString <*> arbitrary <*> arbitrary

instance PTx.ToData LRPDatum where
    toBuiltinData LRPDatum{ldOwner, ldIAsset, ldMaxPrice, ldLovelacesToSpend} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkB ldOwner
            , PlutusTx.mkB ldIAsset
            , PTx.toBuiltinData ldMaxPrice
            , PlutusTx.mkI ldLovelacesToSpend
            ]

instance PTx.FromData LRPDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just ldOwner, D.getB -> Just ldIAsset, maxPrice, D.getI -> Just ldLovelacesToSpend]) ->
            LRPDatum
                <$> pure ldOwner
                <*> pure ldIAsset
                <*> PTx.fromBuiltinData maxPrice
                <*> pure ldLovelacesToSpend
        _ -> Nothing
