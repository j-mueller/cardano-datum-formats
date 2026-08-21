{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Protocol.WingRiders.V2.Request (
    Credential (..),
    StakingCredential (..),
    Address (..),
    SwapDirection (..),
    DatumType (..),
    RequestAction (..),
    RequestDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.WingRiders.V2.Common
import Data.ByteString qualified as BS
import Data.Word (Word8)
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data SwapDirection
    = SwapAToB
    | SwapBToA
    deriving stock (Eq, Show, Generic)

instance Arbitrary SwapDirection where
    arbitrary = Gen.elements [SwapAToB, SwapBToA]

instance PlutusTx.ToData SwapDirection where
    toBuiltinData = \case
        SwapAToB -> PlutusTx.mkConstr 0 []
        SwapBToA -> PlutusTx.mkConstr 1 []

instance PlutusTx.FromData SwapDirection where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, []) -> Just SwapAToB
        (1, []) -> Just SwapBToA
        _ -> Nothing

data DatumType
    = NoDatum
    | DatumHash
    | InlineDatum
    deriving stock (Eq, Show, Generic)

instance Arbitrary DatumType where
    arbitrary = Gen.elements [NoDatum, DatumHash, InlineDatum]

instance PlutusTx.ToData DatumType where
    toBuiltinData = \case
        NoDatum -> PlutusTx.mkConstr 0 []
        DatumHash -> PlutusTx.mkConstr 1 []
        InlineDatum -> PlutusTx.mkConstr 2 []

instance PlutusTx.FromData DatumType where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, []) -> Just NoDatum
        (1, []) -> Just DatumHash
        (2, []) -> Just InlineDatum
        _ -> Nothing

data RequestAction
    = Swap SwapDirection Integer
    | AddLiquidity Integer
    | WithdrawLiquidity Integer Integer
    | ExtractTreasury
    | AddStakingRewards
    | ExtractProjectTreasury
    | ExtractReserveTreasury
    deriving stock (Eq, Show, Generic)

instance Arbitrary RequestAction where
    arbitrary =
        Gen.oneof
            [ Swap <$> arbitrary <*> arbitrary
            , AddLiquidity <$> arbitrary
            , WithdrawLiquidity <$> arbitrary <*> arbitrary
            , pure ExtractTreasury
            , pure AddStakingRewards
            , pure ExtractProjectTreasury
            , pure ExtractReserveTreasury
            ]

instance PlutusTx.ToData RequestAction where
    toBuiltinData = \case
        Swap direction minimumOutput -> PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData direction, PlutusTx.mkI minimumOutput]
        AddLiquidity minimumShares -> PlutusTx.mkConstr 1 [PlutusTx.mkI minimumShares]
        WithdrawLiquidity minimumA minimumB -> PlutusTx.mkConstr 2 [PlutusTx.mkI minimumA, PlutusTx.mkI minimumB]
        ExtractTreasury -> PlutusTx.mkConstr 3 []
        AddStakingRewards -> PlutusTx.mkConstr 4 []
        ExtractProjectTreasury -> PlutusTx.mkConstr 5 []
        ExtractReserveTreasury -> PlutusTx.mkConstr 6 []

instance PlutusTx.FromData RequestAction where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [direction, D.getI -> Just minimumOutput]) -> Swap <$> PlutusTx.fromBuiltinData direction <*> pure minimumOutput
        (1, [D.getI -> Just minimumShares]) -> Just (AddLiquidity minimumShares)
        (2, [D.getI -> Just minimumA, D.getI -> Just minimumB]) -> Just (WithdrawLiquidity minimumA minimumB)
        (3, []) -> Just ExtractTreasury
        (4, []) -> Just AddStakingRewards
        (5, []) -> Just ExtractProjectTreasury
        (6, []) -> Just ExtractReserveTreasury
        _ -> Nothing

data RequestDatum = RequestDatum
    { wrrdOil :: Integer
    , wrrdBeneficiary :: Address
    , wrrdOwnerAddress :: Address
    , wrrdCompensationDatum :: PlutusTx.BuiltinData
    , wrrdCompensationDatumType :: DatumType
    , wrrdDeadline :: Integer
    , wrrdAssetASymbol :: PlutusTx.BuiltinByteString
    , wrrdAssetAToken :: PlutusTx.BuiltinByteString
    , wrrdAssetBSymbol :: PlutusTx.BuiltinByteString
    , wrrdAssetBToken :: PlutusTx.BuiltinByteString
    , wrrdAction :: RequestAction
    , wrrdAScale :: Integer
    , wrrdBScale :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary RequestDatum where
    arbitrary =
        RequestDatum
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> Gen.elements [PlutusTx.mkConstr 0 [], PlutusTx.mkList [], PlutusTx.mkI 0]
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryBuiltinByteString
            <*> arbitraryBuiltinByteString
            <*> arbitraryBuiltinByteString
            <*> arbitraryBuiltinByteString
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance PlutusTx.ToData RequestDatum where
    toBuiltinData RequestDatum{..} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkI wrrdOil
            , PlutusTx.toBuiltinData wrrdBeneficiary
            , PlutusTx.toBuiltinData wrrdOwnerAddress
            , wrrdCompensationDatum
            , PlutusTx.toBuiltinData wrrdCompensationDatumType
            , PlutusTx.mkI wrrdDeadline
            , PlutusTx.mkB wrrdAssetASymbol
            , PlutusTx.mkB wrrdAssetAToken
            , PlutusTx.mkB wrrdAssetBSymbol
            , PlutusTx.mkB wrrdAssetBToken
            , PlutusTx.toBuiltinData wrrdAction
            , PlutusTx.mkI wrrdAScale
            , PlutusTx.mkI wrrdBScale
            ]

instance PlutusTx.FromData RequestDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        ( 0
            , [ oil
              , beneficiary
              , ownerAddress
              , compensationDatum
              , compensationDatumType
              , deadline
              , D.getB -> Just assetASymbol
              , D.getB -> Just assetAToken
              , D.getB -> Just assetBSymbol
              , D.getB -> Just assetBToken
              , action
              , aScale
              , bScale
              ]
            ) ->
                RequestDatum
                    <$> D.getI oil
                    <*> PlutusTx.fromBuiltinData beneficiary
                    <*> PlutusTx.fromBuiltinData ownerAddress
                    <*> pure compensationDatum
                    <*> PlutusTx.fromBuiltinData compensationDatumType
                    <*> D.getI deadline
                    <*> pure assetASymbol
                    <*> pure assetAToken
                    <*> pure assetBSymbol
                    <*> pure assetBToken
                    <*> PlutusTx.fromBuiltinData action
                    <*> D.getI aScale
                    <*> D.getI bScale
        _ -> Nothing

arbitraryBuiltinByteString :: QC.Gen PlutusTx.BuiltinByteString
arbitraryBuiltinByteString = PlutusTx.toBuiltin . BS.pack <$> (arbitrary :: QC.Gen [Word8])
