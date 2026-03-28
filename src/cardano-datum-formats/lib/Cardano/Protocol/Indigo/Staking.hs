{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Indigo.Staking (
    RewardSnapshot (..),
    StakingDatum (..),
    StakingManagerContent (..),
    StakingPositionContent (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions, stripFieldPrefix, sumOptionsWithFieldModifier)
import Cardano.Protocol.Indigo.Common (arbitraryAnyBuiltinByteString, fromMap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

newtype RewardSnapshot = RewardSnapshot
    { rsSnapshotAda :: Integer
    }
    deriving stock (Eq, Show, Generic)

data StakingManagerContent = StakingManagerContent
    { smcTotalStake :: Integer
    , smcManagerSnapshot :: RewardSnapshot
    }
    deriving stock (Eq, Show, Generic)

data StakingPositionContent = StakingPositionContent
    { spcOwner :: PlutusTx.BuiltinByteString
    , spcLockedAmount :: Map Integer (Integer, Integer)
    , spcPositionSnapshot :: RewardSnapshot
    }
    deriving stock (Eq, Show, Generic)

data StakingDatum
    = StakingManager
        { sdManagerContent :: StakingManagerContent
        }
    | StakingPosition
        { sdPositionContent :: StakingPositionContent
        }
    deriving stock (Eq, Show, Generic)

instance Arbitrary RewardSnapshot where
    arbitrary = RewardSnapshot <$> arbitrary

instance Arbitrary StakingManagerContent where
    arbitrary = StakingManagerContent <$> arbitrary <*> arbitrary

instance Arbitrary StakingPositionContent where
    arbitrary = StakingPositionContent <$> arbitraryAnyBuiltinByteString <*> arbitrary <*> arbitrary

instance Arbitrary StakingDatum where
    arbitrary =
        Gen.oneof
            [ StakingManager <$> arbitrary
            , StakingPosition <$> arbitrary
            ]

instance PTx.ToData RewardSnapshot where
    toBuiltinData RewardSnapshot{rsSnapshotAda} =
        PlutusTx.mkConstr 0 [PlutusTx.mkI rsSnapshotAda]

instance PTx.FromData RewardSnapshot where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just rsSnapshotAda]) -> Just RewardSnapshot{rsSnapshotAda}
        _ -> Nothing

instance PTx.ToData StakingManagerContent where
    toBuiltinData StakingManagerContent{smcTotalStake, smcManagerSnapshot} =
        PlutusTx.mkConstr 0 [PlutusTx.mkI smcTotalStake, PTx.toBuiltinData smcManagerSnapshot]

instance PTx.FromData StakingManagerContent where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just smcTotalStake, managerSnapshot]) ->
            StakingManagerContent <$> pure smcTotalStake <*> PTx.fromBuiltinData managerSnapshot
        _ -> Nothing

instance PTx.ToData StakingPositionContent where
    toBuiltinData StakingPositionContent{spcOwner, spcLockedAmount, spcPositionSnapshot} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkB spcOwner
            , PlutusTx.mkMap
                [ ( PlutusTx.mkI lockKey
                  , PlutusTx.mkConstr 0 [PlutusTx.mkI lockAmount, PlutusTx.mkI lockDeadline]
                  )
                | (lockKey, (lockAmount, lockDeadline)) <- Map.toAscList spcLockedAmount
                ]
            , PTx.toBuiltinData spcPositionSnapshot
            ]

instance PTx.FromData StakingPositionContent where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just spcOwner, lockedAmount, positionSnapshot]) -> do
            entries <- fromMap lockedAmount
            spcLockedAmount <-
                traverse
                    (\(keyData, valueData) -> do
                        lockKey <- D.getI keyData
                        pair <- D.withConstr valueData $ \case
                            (0, [D.getI -> Just lockAmount, D.getI -> Just lockDeadline]) ->
                                Just (lockAmount, lockDeadline)
                            _ -> Nothing
                        pure (lockKey, pair)
                    )
                    entries
            StakingPositionContent
                <$> pure spcOwner
                <*> pure (Map.fromList spcLockedAmount)
                <*> PTx.fromBuiltinData positionSnapshot
        _ -> Nothing

instance PTx.ToData StakingDatum where
    toBuiltinData = \case
        StakingManager sdManagerContent ->
            PlutusTx.mkConstr 0 [stagingManagerContentToVariantData sdManagerContent]
        StakingPosition sdPositionContent ->
            PlutusTx.mkConstr 1 [stakingPositionContentToVariantData sdPositionContent]

instance PTx.FromData StakingDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [manager]) ->
            StakingManager <$> stakingManagerContentFromVariantData manager
        (1, [position]) ->
            StakingPosition <$> stakingPositionContentFromVariantData position
        _ -> Nothing

stagingManagerContentToVariantData :: StakingManagerContent -> PlutusTx.BuiltinData
stagingManagerContentToVariantData StakingManagerContent{smcTotalStake, smcManagerSnapshot} =
    PlutusTx.mkConstr 0 [PlutusTx.mkI smcTotalStake, PTx.toBuiltinData smcManagerSnapshot]

stakingManagerContentFromVariantData :: PlutusTx.BuiltinData -> Maybe StakingManagerContent
stakingManagerContentFromVariantData dt = D.withConstr dt $ \case
    (0, [D.getI -> Just smcTotalStake, managerSnapshot]) ->
        StakingManagerContent <$> pure smcTotalStake <*> PTx.fromBuiltinData managerSnapshot
    _ -> Nothing

stakingPositionContentToVariantData :: StakingPositionContent -> PlutusTx.BuiltinData
stakingPositionContentToVariantData StakingPositionContent{spcOwner, spcLockedAmount, spcPositionSnapshot} =
    PlutusTx.mkConstr
        0
        [ PlutusTx.mkB spcOwner
        , PlutusTx.mkMap
            [ ( PlutusTx.mkI lockKey
              , PlutusTx.mkConstr 0 [PlutusTx.mkI lockAmount, PlutusTx.mkI lockDeadline]
              )
            | (lockKey, (lockAmount, lockDeadline)) <- Map.toAscList spcLockedAmount
            ]
        , PTx.toBuiltinData spcPositionSnapshot
        ]

stakingPositionContentFromVariantData :: PlutusTx.BuiltinData -> Maybe StakingPositionContent
stakingPositionContentFromVariantData dt = D.withConstr dt $ \case
    (0, [D.getB -> Just spcOwner, lockedAmount, positionSnapshot]) -> do
        entries <- fromMap lockedAmount
        spcLockedAmount <-
            traverse
                (\(keyData, valueData) -> do
                    lockKey <- D.getI keyData
                    pair <- D.withConstr valueData $ \case
                        (0, [D.getI -> Just lockAmount, D.getI -> Just lockDeadline]) ->
                            Just (lockAmount, lockDeadline)
                        _ -> Nothing
                    pure (lockKey, pair)
                )
                entries
        StakingPositionContent
            <$> pure spcOwner
            <*> pure (Map.fromList spcLockedAmount)
            <*> PTx.fromBuiltinData positionSnapshot
    _ -> Nothing

instance ToJSON RewardSnapshot where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON RewardSnapshot where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''RewardSnapshot)

instance Schema.ToSchema RewardSnapshot where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSON StakingManagerContent where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON StakingManagerContent where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''StakingManagerContent)

instance Schema.ToSchema StakingManagerContent where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))

instance ToJSON StakingPositionContent where
    toJSON = Aeson.genericToJSON (jsonOptions 3)
    toEncoding = Aeson.genericToEncoding (jsonOptions 3)

instance FromJSON StakingPositionContent where
    parseJSON = Aeson.genericParseJSON (jsonOptions 3)

$(deriveTypeScript (jsonOptions 3) ''StakingPositionContent)

instance Schema.ToSchema StakingPositionContent where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 3))

instance ToJSON StakingDatum where
    toJSON = Aeson.genericToJSON (sumOptionsWithFieldModifier 0 (stripFieldPrefix "sd"))
    toEncoding = Aeson.genericToEncoding (sumOptionsWithFieldModifier 0 (stripFieldPrefix "sd"))

instance FromJSON StakingDatum where
    parseJSON = Aeson.genericParseJSON (sumOptionsWithFieldModifier 0 (stripFieldPrefix "sd"))

$(deriveTypeScript (sumOptionsWithFieldModifier 0 (stripFieldPrefix "sd")) ''StakingDatum)

instance Schema.ToSchema StakingDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptionsWithFieldModifier 0 (stripFieldPrefix "sd")))
