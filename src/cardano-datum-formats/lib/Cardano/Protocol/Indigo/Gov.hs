{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Indigo.Gov (
    GovDatum (..),
    ProtocolParams (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.Indigo.Common (OnChainDecimal)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
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
    deriving stock (Eq, Show, Generic)

data GovDatum = GovDatum
    { gdCurrentProposal :: Integer
    , gdProtocolParams :: ProtocolParams
    , gdCurrentVersion :: Integer
    , gdIAssetsCount :: Integer
    , gdActiveProposals :: Integer
    , gdTreasuryIndyWithdrawnAmt :: Integer
    }
    deriving stock (Eq, Show, Generic)

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

instance ToJSON ProtocolParams where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON ProtocolParams where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''ProtocolParams)

instance Schema.ToSchema ProtocolParams where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSON GovDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON GovDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''GovDatum)

instance Schema.ToSchema GovDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
