{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Protocol.WingRiders.V2.Pool (
    PoolDatum (..),
    ConstantProductPoolDatum (..),
    StableswapPoolDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.WingRiders.V2.Common (Address)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data PoolDatum poolSpecifics = PoolDatum
    { wrpdRequestValidatorHash :: PlutusTx.BuiltinByteString
    , wrpdAssetASymbol :: PlutusTx.BuiltinByteString
    , wrpdAssetAToken :: PlutusTx.BuiltinByteString
    , wrpdAssetBSymbol :: PlutusTx.BuiltinByteString
    , wrpdAssetBToken :: PlutusTx.BuiltinByteString
    , wrpdSwapFeeInBasis :: Integer
    , wrpdProtocolFeeInBasis :: Integer
    , wrpdProjectFeeInBasis :: Integer
    , wrpdReserveFeeInBasis :: Integer
    , wrpdFeeBasis :: Integer
    , wrpdAgentFeeAda :: Integer
    , wrpdLastInteraction :: Integer
    , wrpdTreasuryA :: Integer
    , wrpdTreasuryB :: Integer
    , wrpdProjectTreasuryA :: Integer
    , wrpdProjectTreasuryB :: Integer
    , wrpdReserveTreasuryA :: Integer
    , wrpdReserveTreasuryB :: Integer
    , wrpdProjectBeneficiary :: Maybe Address
    , wrpdReserveBeneficiary :: Maybe Address
    , wrpdPoolSpecifics :: poolSpecifics
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary poolSpecifics => Arbitrary (PoolDatum poolSpecifics) where
    arbitrary =
        PoolDatum
            <$> arbitraryBuiltinByteString
            <*> arbitraryBuiltinByteString
            <*> arbitraryBuiltinByteString
            <*> arbitraryBuiltinByteString
            <*> arbitraryBuiltinByteString
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance PlutusTx.ToData poolSpecifics => PlutusTx.ToData (PoolDatum poolSpecifics) where
    toBuiltinData PoolDatum{..} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkB wrpdRequestValidatorHash
            , PlutusTx.mkB wrpdAssetASymbol
            , PlutusTx.mkB wrpdAssetAToken
            , PlutusTx.mkB wrpdAssetBSymbol
            , PlutusTx.mkB wrpdAssetBToken
            , PlutusTx.mkI wrpdSwapFeeInBasis
            , PlutusTx.mkI wrpdProtocolFeeInBasis
            , PlutusTx.mkI wrpdProjectFeeInBasis
            , PlutusTx.mkI wrpdReserveFeeInBasis
            , PlutusTx.mkI wrpdFeeBasis
            , PlutusTx.mkI wrpdAgentFeeAda
            , PlutusTx.mkI wrpdLastInteraction
            , PlutusTx.mkI wrpdTreasuryA
            , PlutusTx.mkI wrpdTreasuryB
            , PlutusTx.mkI wrpdProjectTreasuryA
            , PlutusTx.mkI wrpdProjectTreasuryB
            , PlutusTx.mkI wrpdReserveTreasuryA
            , PlutusTx.mkI wrpdReserveTreasuryB
            , PlutusTx.toBuiltinData wrpdProjectBeneficiary
            , PlutusTx.toBuiltinData wrpdReserveBeneficiary
            , PlutusTx.toBuiltinData wrpdPoolSpecifics
            ]

instance PlutusTx.FromData poolSpecifics => PlutusTx.FromData (PoolDatum poolSpecifics) where
    fromBuiltinData dt = D.withConstr dt $ \case
        ( 0
            , [ D.getB -> Just wrpdRequestValidatorHash
              , D.getB -> Just wrpdAssetASymbol
              , D.getB -> Just wrpdAssetAToken
              , D.getB -> Just wrpdAssetBSymbol
              , D.getB -> Just wrpdAssetBToken
              , D.getI -> Just wrpdSwapFeeInBasis
              , D.getI -> Just wrpdProtocolFeeInBasis
              , D.getI -> Just wrpdProjectFeeInBasis
              , D.getI -> Just wrpdReserveFeeInBasis
              , D.getI -> Just wrpdFeeBasis
              , D.getI -> Just wrpdAgentFeeAda
              , D.getI -> Just wrpdLastInteraction
              , D.getI -> Just wrpdTreasuryA
              , D.getI -> Just wrpdTreasuryB
              , D.getI -> Just wrpdProjectTreasuryA
              , D.getI -> Just wrpdProjectTreasuryB
              , D.getI -> Just wrpdReserveTreasuryA
              , D.getI -> Just wrpdReserveTreasuryB
              , projectBeneficiary
              , reserveBeneficiary
              , poolSpecifics
              ]
            ) ->
                PoolDatum
                    <$> pure wrpdRequestValidatorHash
                    <*> pure wrpdAssetASymbol
                    <*> pure wrpdAssetAToken
                    <*> pure wrpdAssetBSymbol
                    <*> pure wrpdAssetBToken
                    <*> pure wrpdSwapFeeInBasis
                    <*> pure wrpdProtocolFeeInBasis
                    <*> pure wrpdProjectFeeInBasis
                    <*> pure wrpdReserveFeeInBasis
                    <*> pure wrpdFeeBasis
                    <*> pure wrpdAgentFeeAda
                    <*> pure wrpdLastInteraction
                    <*> pure wrpdTreasuryA
                    <*> pure wrpdTreasuryB
                    <*> pure wrpdProjectTreasuryA
                    <*> pure wrpdProjectTreasuryB
                    <*> pure wrpdReserveTreasuryA
                    <*> pure wrpdReserveTreasuryB
                    <*> PlutusTx.fromBuiltinData projectBeneficiary
                    <*> PlutusTx.fromBuiltinData reserveBeneficiary
                    <*> PlutusTx.fromBuiltinData poolSpecifics
        _ -> Nothing

data ConstantProductPoolDatum = ConstantProductPoolDatum
    deriving stock (Eq, Show, Generic)

instance Arbitrary ConstantProductPoolDatum where
    arbitrary = pure ConstantProductPoolDatum

instance PlutusTx.ToData ConstantProductPoolDatum where
    toBuiltinData ConstantProductPoolDatum = PlutusTx.mkConstr 0 []

instance PlutusTx.FromData ConstantProductPoolDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, []) -> Just ConstantProductPoolDatum
        _ -> Nothing

data StableswapPoolDatum = StableswapPoolDatum
    { wrspdParameterD :: Integer
    , wrspdAScale :: Integer
    , wrspdBScale :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary StableswapPoolDatum where
    arbitrary = StableswapPoolDatum <$> arbitrary <*> arbitrary <*> arbitrary

instance PlutusTx.ToData StableswapPoolDatum where
    toBuiltinData StableswapPoolDatum{wrspdParameterD, wrspdAScale, wrspdBScale} =
        PlutusTx.mkConstr 0 [PlutusTx.mkI wrspdParameterD, PlutusTx.mkI wrspdAScale, PlutusTx.mkI wrspdBScale]

instance PlutusTx.FromData StableswapPoolDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just wrspdParameterD, D.getI -> Just wrspdAScale, D.getI -> Just wrspdBScale]) ->
            Just StableswapPoolDatum{wrspdParameterD, wrspdAScale, wrspdBScale}
        _ -> Nothing

arbitraryBuiltinByteString :: QC.Gen PlutusTx.BuiltinByteString
arbitraryBuiltinByteString = PlutusTx.toBuiltin . BS.pack <$> (arbitrary :: QC.Gen [Word8])
