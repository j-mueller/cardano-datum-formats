module Cardano.Protocol.Indigo.Gov (
    GovDatum (..),
    ProtocolParams (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.Indigo.Common (OnChainDecimal)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data ProtocolParams = ProtocolParams
    { ppProposalDeposit :: Integer
    , ppVotingPeriod :: Integer
    , ppEffectiveDelay :: Integer
    , ppExpirationPeriod :: Integer
    , ppCollateralFeePercentage :: OnChainDecimal
    , ppProposingPeriod :: Integer
    , ppTotalShards :: Integer
    , ppMinimumQuorum :: Integer
    , ppMaxTreasuryLovelaceSpend :: Integer
    , ppMaxTreasuryIndySpend :: Integer
    }
    deriving stock (Eq, Show)

data GovDatum = GovDatum
    { gdCurrentProposal :: Integer
    , gdProtocolParams :: ProtocolParams
    , gdCurrentVersion :: Integer
    , gdIAssetsCount :: Integer
    , gdActiveProposals :: Integer
    , gdTreasuryIndyWithdrawnAmt :: Integer
    }
    deriving stock (Eq, Show)

instance Arbitrary ProtocolParams where
    arbitrary =
        ProtocolParams
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary GovDatum where
    arbitrary =
        GovDatum
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance PTx.ToData ProtocolParams where
    toBuiltinData ProtocolParams{ppProposalDeposit, ppVotingPeriod, ppEffectiveDelay, ppExpirationPeriod, ppCollateralFeePercentage, ppProposingPeriod, ppTotalShards, ppMinimumQuorum, ppMaxTreasuryLovelaceSpend, ppMaxTreasuryIndySpend} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkI ppProposalDeposit
            , PlutusTx.mkI ppVotingPeriod
            , PlutusTx.mkI ppEffectiveDelay
            , PlutusTx.mkI ppExpirationPeriod
            , PTx.toBuiltinData ppCollateralFeePercentage
            , PlutusTx.mkI ppProposingPeriod
            , PlutusTx.mkI ppTotalShards
            , PlutusTx.mkI ppMinimumQuorum
            , PlutusTx.mkI ppMaxTreasuryLovelaceSpend
            , PlutusTx.mkI ppMaxTreasuryIndySpend
            ]

instance PTx.FromData ProtocolParams where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just ppProposalDeposit, D.getI -> Just ppVotingPeriod, D.getI -> Just ppEffectiveDelay, D.getI -> Just ppExpirationPeriod, collateralFeePercentage, D.getI -> Just ppProposingPeriod, D.getI -> Just ppTotalShards, D.getI -> Just ppMinimumQuorum, D.getI -> Just ppMaxTreasuryLovelaceSpend, D.getI -> Just ppMaxTreasuryIndySpend]) ->
            ProtocolParams
                <$> pure ppProposalDeposit
                <*> pure ppVotingPeriod
                <*> pure ppEffectiveDelay
                <*> pure ppExpirationPeriod
                <*> PTx.fromBuiltinData collateralFeePercentage
                <*> pure ppProposingPeriod
                <*> pure ppTotalShards
                <*> pure ppMinimumQuorum
                <*> pure ppMaxTreasuryLovelaceSpend
                <*> pure ppMaxTreasuryIndySpend
        _ -> Nothing

instance PTx.ToData GovDatum where
    toBuiltinData GovDatum{gdCurrentProposal, gdProtocolParams, gdCurrentVersion, gdIAssetsCount, gdActiveProposals, gdTreasuryIndyWithdrawnAmt} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkI gdCurrentProposal
            , PTx.toBuiltinData gdProtocolParams
            , PlutusTx.mkI gdCurrentVersion
            , PlutusTx.mkI gdIAssetsCount
            , PlutusTx.mkI gdActiveProposals
            , PlutusTx.mkI gdTreasuryIndyWithdrawnAmt
            ]

instance PTx.FromData GovDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just gdCurrentProposal, protocolParams, D.getI -> Just gdCurrentVersion, D.getI -> Just gdIAssetsCount, D.getI -> Just gdActiveProposals, D.getI -> Just gdTreasuryIndyWithdrawnAmt]) ->
            GovDatum
                <$> pure gdCurrentProposal
                <*> PTx.fromBuiltinData protocolParams
                <*> pure gdCurrentVersion
                <*> pure gdIAssetsCount
                <*> pure gdActiveProposals
                <*> pure gdTreasuryIndyWithdrawnAmt
        _ -> Nothing
