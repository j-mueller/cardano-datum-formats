module Cardano.Protocol.Indigo.PriceOracle (
    OracleAssetNft (..),
    PriceOracleDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.Indigo.Common (AssetClass, OnChainDecimal)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

newtype OracleAssetNft = OracleAssetNft
    { oanAsset :: AssetClass
    }
    deriving stock (Eq, Show)

data PriceOracleDatum = PriceOracleDatum
    { podPrice :: OnChainDecimal
    , podExpiration :: Integer
    }
    deriving stock (Eq, Show)

instance Arbitrary OracleAssetNft where
    arbitrary = OracleAssetNft <$> arbitrary

instance Arbitrary PriceOracleDatum where
    arbitrary = PriceOracleDatum <$> arbitrary <*> arbitrary

instance PTx.ToData OracleAssetNft where
    toBuiltinData OracleAssetNft{oanAsset} =
        PlutusTx.mkConstr 0 [PTx.toBuiltinData oanAsset]

instance PTx.FromData OracleAssetNft where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [asset]) ->
            OracleAssetNft <$> PTx.fromBuiltinData asset
        _ -> Nothing

instance PTx.ToData PriceOracleDatum where
    toBuiltinData PriceOracleDatum{podPrice, podExpiration} =
        PlutusTx.mkConstr 0 [PTx.toBuiltinData podPrice, PlutusTx.mkI podExpiration]

instance PTx.FromData PriceOracleDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [price, D.getI -> Just podExpiration]) ->
            PriceOracleDatum <$> PTx.fromBuiltinData price <*> pure podExpiration
        _ -> Nothing
