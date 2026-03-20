module Cardano.Protocol.Indigo.Staking (
    RewardSnapshot (..),
    StakingDatum (..),
    StakingManagerContent (..),
    StakingPositionContent (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.Indigo.Common (arbitraryAnyBuiltinByteString, fromMap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

newtype RewardSnapshot = RewardSnapshot
    { rsSnapshotAda :: Integer
    }
    deriving stock (Eq, Show)

data StakingManagerContent = StakingManagerContent
    { smcTotalStake :: Integer
    , smcManagerSnapshot :: RewardSnapshot
    }
    deriving stock (Eq, Show)

data StakingPositionContent = StakingPositionContent
    { spcOwner :: PlutusTx.BuiltinByteString
    , spcLockedAmount :: Map Integer (Integer, Integer)
    , spcPositionSnapshot :: RewardSnapshot
    }
    deriving stock (Eq, Show)

data StakingDatum
    = StakingManager
        { sdManagerContent :: StakingManagerContent
        }
    | StakingPosition
        { sdPositionContent :: StakingPositionContent
        }
    deriving stock (Eq, Show)

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
