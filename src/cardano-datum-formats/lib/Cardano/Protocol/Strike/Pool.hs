module Cardano.Protocol.Strike.Pool (
    PoolDatum (..),
) where

import Cardano.Asset (Asset)
import Cardano.Data qualified as D
import Cardano.Protocol.Strike.Common (arbitraryAnyBuiltinByteString)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data PoolDatum = PoolDatum
    { pdUnderlyingAsset :: Asset
    , pdLpAsset :: Asset
    , pdLiquidityTotalAssetAmount :: Integer
    , pdLiquidityTotalLpMinted :: Integer
    , pdTotalLendedAmount :: Integer
    , pdBatcherLicense :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Show)

instance Arbitrary PoolDatum where
    arbitrary =
        PoolDatum
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryAnyBuiltinByteString

instance PTx.ToData PoolDatum where
    toBuiltinData PoolDatum{pdUnderlyingAsset, pdLpAsset, pdLiquidityTotalAssetAmount, pdLiquidityTotalLpMinted, pdTotalLendedAmount, pdBatcherLicense} =
        PlutusTx.mkConstr
            0
            [ PTx.toBuiltinData pdUnderlyingAsset
            , PTx.toBuiltinData pdLpAsset
            , PlutusTx.mkI pdLiquidityTotalAssetAmount
            , PlutusTx.mkI pdLiquidityTotalLpMinted
            , PlutusTx.mkI pdTotalLendedAmount
            , PlutusTx.mkB pdBatcherLicense
            ]

instance PTx.FromData PoolDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [underlyingAsset, lpAsset, D.getI -> Just pdLiquidityTotalAssetAmount, D.getI -> Just pdLiquidityTotalLpMinted, D.getI -> Just pdTotalLendedAmount, D.getB -> Just pdBatcherLicense]) ->
            PoolDatum
                <$> PTx.fromBuiltinData underlyingAsset
                <*> PTx.fromBuiltinData lpAsset
                <*> pure pdLiquidityTotalAssetAmount
                <*> pure pdLiquidityTotalLpMinted
                <*> pure pdTotalLendedAmount
                <*> pure pdBatcherLicense
        _ -> Nothing
