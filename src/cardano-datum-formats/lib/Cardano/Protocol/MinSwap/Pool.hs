{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Protocol.MinSwap.Pool (
    PoolFeeSharing (..),
    PoolDatumV1 (..),
    BaseFee (..),
    DynamicFee (..),
    PoolDatumV2 (..),
    poolPair,
    poolId,
) where

import Cardano.Address.Plutus qualified as Address
import Cardano.Api qualified as C
import Cardano.Asset qualified as Asset
import Cardano.Asset.Pair (Pair, lpAssetName, pair)
import Cardano.Data qualified as D
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Hedgehog qualified as H

data PoolFeeSharing addr = PoolFeeSharing
    { pfTo :: addr
    , pfDatumHash :: C.Hash C.ScriptData
    }
    deriving stock (Eq, Show, Functor)

instance Arbitrary addr => Arbitrary (PoolFeeSharing addr) where
    arbitrary =
        PoolFeeSharing
            <$> arbitrary
            <*> H.hedgehog Gen.genHashScriptData

instance PlutusTx.ToData addr => PlutusTx.ToData (PoolFeeSharing addr) where
    toBuiltinData PoolFeeSharing{pfTo, pfDatumHash} =
        PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData pfTo, D.serialiseHash pfDatumHash]

instance PlutusTx.FromData addr => PlutusTx.FromData (PoolFeeSharing addr) where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [to, datumHash]) ->
            PoolFeeSharing <$> PlutusTx.fromBuiltinData to <*> (D.getB datumHash >>= D.deserialiseHash)
        _ -> Nothing

data PoolDatumV1 addr = PoolDatumV1
    { pd1AssetA :: C.AssetId
    , pd1AssetB :: C.AssetId
    , pd1TotalLiquidity :: C.Quantity
    , pd1RootKLast :: C.Quantity
    , pd1FeeSharing :: Maybe (PoolFeeSharing addr)
    }
    deriving stock (Eq, Show, Functor)

instance Arbitrary addr => Arbitrary (PoolDatumV1 addr) where
    arbitrary =
        PoolDatumV1
            <$> H.hedgehog Gen.genAssetId
            <*> H.hedgehog Gen.genAssetId
            <*> H.hedgehog Gen.genSignedNonZeroQuantity
            <*> H.hedgehog Gen.genSignedNonZeroQuantity
            <*> arbitrary

instance PlutusTx.ToData addr => PlutusTx.ToData (PoolDatumV1 addr) where
    toBuiltinData PoolDatumV1{pd1AssetA, pd1AssetB, pd1TotalLiquidity = C.Quantity liq, pd1RootKLast = C.Quantity rootKLast, pd1FeeSharing} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.toBuiltinData (Asset.fromAssetId pd1AssetA)
            , PlutusTx.toBuiltinData (Asset.fromAssetId pd1AssetB)
            , PlutusTx.toBuiltinData liq
            , PlutusTx.toBuiltinData rootKLast
            , PlutusTx.toBuiltinData pd1FeeSharing
            ]

instance PlutusTx.FromData addr => PlutusTx.FromData (PoolDatumV1 addr) where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [assetA, assetB, totalLiq, rootK, feeSharing]) ->
            PoolDatumV1
                <$> (PlutusTx.fromBuiltinData assetA >>= Asset.toAssetId)
                <*> (PlutusTx.fromBuiltinData assetB >>= Asset.toAssetId)
                <*> fmap C.Quantity (PlutusTx.fromBuiltinData totalLiq)
                <*> fmap C.Quantity (PlutusTx.fromBuiltinData rootK)
                <*> PlutusTx.fromBuiltinData feeSharing
        _ -> Nothing

data BaseFee = BaseFee
    { bfANumerator :: C.Quantity
    , bfBNumerator :: C.Quantity
    }
    deriving stock (Eq, Show)

instance Arbitrary BaseFee where
    arbitrary =
        BaseFee
            <$> H.hedgehog Gen.genPositiveQuantity
            <*> H.hedgehog Gen.genPositiveQuantity

instance PlutusTx.ToData BaseFee where
    toBuiltinData BaseFee{bfANumerator = C.Quantity a, bfBNumerator = C.Quantity b} =
        PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData a, PlutusTx.toBuiltinData b]

instance PlutusTx.FromData BaseFee where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [a, b]) -> BaseFee <$> fmap C.Quantity (PlutusTx.fromBuiltinData a) <*> fmap C.Quantity (PlutusTx.fromBuiltinData b)
        _ -> Nothing

data DynamicFee = AllowDynamicFee | DisallowDynamicFee
    deriving stock (Eq, Show)

instance Arbitrary DynamicFee where
    arbitrary = Gen.elements [AllowDynamicFee, DisallowDynamicFee]

instance PlutusTx.ToData DynamicFee where
    toBuiltinData = \case
        AllowDynamicFee -> PlutusTx.mkConstr 1 []
        DisallowDynamicFee -> PlutusTx.mkConstr 0 []

instance PlutusTx.FromData DynamicFee where
    fromBuiltinData k = D.withConstr k $ \case
        (0, []) -> Just DisallowDynamicFee
        (1, []) -> Just AllowDynamicFee
        _ -> Nothing

data PoolDatumV2 = PoolDatumV2
    { pd2BatchingStakeCredential :: C.StakeCredential
    , pd2AssetA :: C.AssetId
    , pd2AssetB :: C.AssetId
    , pd2TotalLiquidity :: C.Quantity
    , pd2ReserveA :: C.Quantity
    , pd2ReserveB :: C.Quantity
    , pd2BaseFee :: BaseFee
    , pd2FeeSharingNumerator :: Maybe C.Quantity
    , pd2AllowDynamicFee :: DynamicFee
    }
    deriving stock (Eq, Show)

instance Arbitrary PoolDatumV2 where
    arbitrary =
        PoolDatumV2
            <$> H.hedgehog Gen.genStakeCredential
            <*> H.hedgehog Gen.genAssetId
            <*> H.hedgehog Gen.genAssetId
            <*> H.hedgehog Gen.genSignedNonZeroQuantity
            <*> H.hedgehog Gen.genSignedNonZeroQuantity
            <*> H.hedgehog Gen.genSignedNonZeroQuantity
            <*> arbitrary
            <*> Gen.oneof [Just <$> H.hedgehog Gen.genPositiveQuantity, pure Nothing]
            <*> arbitrary

instance PlutusTx.ToData PoolDatumV2 where
    toBuiltinData PoolDatumV2{pd2BatchingStakeCredential, pd2AssetA, pd2AssetB, pd2TotalLiquidity = C.Quantity totalLiq, pd2ReserveA = C.Quantity reserveA, pd2ReserveB = C.Quantity reserveB, pd2BaseFee = BaseFee (C.Quantity baseFeeA) (C.Quantity baseFeeB), pd2FeeSharingNumerator, pd2AllowDynamicFee} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.toBuiltinData (Address.PlutusStakeCredential pd2BatchingStakeCredential)
            , PlutusTx.toBuiltinData (Asset.fromAssetId pd2AssetA)
            , PlutusTx.toBuiltinData (Asset.fromAssetId pd2AssetB)
            , PlutusTx.toBuiltinData totalLiq
            , PlutusTx.toBuiltinData reserveA
            , PlutusTx.toBuiltinData reserveB
            , PlutusTx.toBuiltinData baseFeeA
            , PlutusTx.toBuiltinData baseFeeB
            , PlutusTx.toBuiltinData ((\(C.Quantity n) -> n) <$> pd2FeeSharingNumerator)
            , PlutusTx.toBuiltinData pd2AllowDynamicFee
            ]

instance PlutusTx.FromData PoolDatumV2 where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [batchingCred, assetA, assetB, totalLiq, reserveA, reserveB, baseFeeA, baseFeeB, poolSharingNum, allowDynamic]) ->
            PoolDatumV2
                <$> (Address.getPlutusStakeCredential <$> PlutusTx.fromBuiltinData batchingCred)
                <*> (PlutusTx.fromBuiltinData assetA >>= Asset.toAssetId)
                <*> (PlutusTx.fromBuiltinData assetB >>= Asset.toAssetId)
                <*> fmap C.Quantity (PlutusTx.fromBuiltinData totalLiq)
                <*> fmap C.Quantity (PlutusTx.fromBuiltinData reserveA)
                <*> fmap C.Quantity (PlutusTx.fromBuiltinData reserveB)
                <*> (BaseFee <$> fmap C.Quantity (PlutusTx.fromBuiltinData baseFeeA) <*> fmap C.Quantity (PlutusTx.fromBuiltinData baseFeeB))
                <*> fmap (fmap C.Quantity) (PlutusTx.fromBuiltinData poolSharingNum)
                <*> PlutusTx.fromBuiltinData allowDynamic
        _ -> Nothing

poolPair :: PoolDatumV2 -> Pair
poolPair PoolDatumV2{pd2AssetA, pd2AssetB} =
    pair pd2AssetA pd2AssetB

poolId :: PoolDatumV2 -> C.AssetName
poolId = lpAssetName . poolPair
