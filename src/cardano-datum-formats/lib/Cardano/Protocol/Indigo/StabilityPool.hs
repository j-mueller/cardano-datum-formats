module Cardano.Protocol.Indigo.StabilityPool (
    AccountAction (..),
    AccountContent (..),
    SnapshotEpochToScaleToSumContent (..),
    StabilityPoolContent (..),
    StabilityPoolDatum (..),
    StabilityPoolSnapshot (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.Indigo.Common (
    Address,
    EpochToScaleKey,
    SPInteger,
    arbitraryAnyBuiltinByteString,
    fromMap,
    maybeFromOptionData,
    maybeToOptionData,
 )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data StabilityPoolSnapshot = StabilityPoolSnapshot
    { spsProductVal :: SPInteger
    , spsDepositVal :: SPInteger
    , spsSumVal :: SPInteger
    , spsEpoch :: Integer
    , spsScale :: Integer
    }
    deriving stock (Eq, Show)

data StabilityPoolContent = StabilityPoolContent
    { spcAsset :: PlutusTx.BuiltinByteString
    , spcSnapshot :: StabilityPoolSnapshot
    , spcEpochToScaleToSum :: Map EpochToScaleKey SPInteger
    }
    deriving stock (Eq, Show)

data AccountAction
    = Create
    | Adjust
        { aaAmount :: Integer
        , aaOutputAddress :: Address
        }
    | Close
        { aaOutputAddress :: Address
        }
    deriving stock (Eq, Show)

data AccountContent = AccountContent
    { acOwner :: PlutusTx.BuiltinByteString
    , acAsset :: PlutusTx.BuiltinByteString
    , acSnapshot :: StabilityPoolSnapshot
    , acRequest :: Maybe AccountAction
    }
    deriving stock (Eq, Show)

data SnapshotEpochToScaleToSumContent = SnapshotEpochToScaleToSumContent
    { sesAsset :: PlutusTx.BuiltinByteString
    , sesSnapshot :: Map EpochToScaleKey SPInteger
    }
    deriving stock (Eq, Show)

data StabilityPoolDatum
    = StabilityPool
        { spdContent :: StabilityPoolContent
        }
    | Account
        { spdAccountContent :: AccountContent
        }
    | SnapshotEpochToScaleToSum
        { spdSnapshotContent :: SnapshotEpochToScaleToSumContent
        }
    deriving stock (Eq, Show)

instance Arbitrary StabilityPoolSnapshot where
    arbitrary =
        StabilityPoolSnapshot
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary StabilityPoolContent where
    arbitrary = StabilityPoolContent <$> arbitraryAnyBuiltinByteString <*> arbitrary <*> arbitrary

instance Arbitrary AccountAction where
    arbitrary =
        Gen.oneof
            [ pure Create
            , Adjust <$> arbitrary <*> arbitrary
            , Close <$> arbitrary
            ]

instance Arbitrary AccountContent where
    arbitrary = AccountContent <$> arbitraryAnyBuiltinByteString <*> arbitraryAnyBuiltinByteString <*> arbitrary <*> arbitrary

instance Arbitrary SnapshotEpochToScaleToSumContent where
    arbitrary = SnapshotEpochToScaleToSumContent <$> arbitraryAnyBuiltinByteString <*> arbitrary

instance Arbitrary StabilityPoolDatum where
    arbitrary =
        Gen.oneof
            [ StabilityPool <$> arbitrary
            , Account <$> arbitrary
            , SnapshotEpochToScaleToSum <$> arbitrary
            ]

instance PTx.ToData StabilityPoolSnapshot where
    toBuiltinData StabilityPoolSnapshot{spsProductVal, spsDepositVal, spsSumVal, spsEpoch, spsScale} =
        PlutusTx.mkConstr
            0
            [ PTx.toBuiltinData spsProductVal
            , PTx.toBuiltinData spsDepositVal
            , PTx.toBuiltinData spsSumVal
            , PlutusTx.mkI spsEpoch
            , PlutusTx.mkI spsScale
            ]

instance PTx.FromData StabilityPoolSnapshot where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [productVal, depositVal, sumVal, D.getI -> Just spsEpoch, D.getI -> Just spsScale]) ->
            StabilityPoolSnapshot
                <$> PTx.fromBuiltinData productVal
                <*> PTx.fromBuiltinData depositVal
                <*> PTx.fromBuiltinData sumVal
                <*> pure spsEpoch
                <*> pure spsScale
        _ -> Nothing

instance PTx.ToData StabilityPoolContent where
    toBuiltinData StabilityPoolContent{spcAsset, spcSnapshot, spcEpochToScaleToSum} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkB spcAsset
            , PTx.toBuiltinData spcSnapshot
            , PlutusTx.mkMap
                [ (PTx.toBuiltinData key, PTx.toBuiltinData value)
                | (key, value) <- Map.toAscList spcEpochToScaleToSum
                ]
            ]

instance PTx.FromData StabilityPoolContent where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just spcAsset, snapshot, epochToScaleToSum]) -> do
            entries <- fromMap epochToScaleToSum
            decodedEntries <-
                traverse
                    (\(keyData, valueData) -> do
                        key <- PTx.fromBuiltinData keyData
                        value <- PTx.fromBuiltinData valueData
                        pure (key, value)
                    )
                    entries
            StabilityPoolContent
                <$> pure spcAsset
                <*> PTx.fromBuiltinData snapshot
                <*> pure (Map.fromList decodedEntries)
        _ -> Nothing

instance PTx.ToData AccountAction where
    toBuiltinData = \case
        Create -> PlutusTx.mkConstr 0 []
        Adjust aaAmount aaOutputAddress ->
            PlutusTx.mkConstr 1 [PlutusTx.mkI aaAmount, PTx.toBuiltinData aaOutputAddress]
        Close aaOutputAddress ->
            PlutusTx.mkConstr 2 [PTx.toBuiltinData aaOutputAddress]

instance PTx.FromData AccountAction where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, []) -> Just Create
        (1, [D.getI -> Just aaAmount, outputAddress]) ->
            Adjust <$> pure aaAmount <*> PTx.fromBuiltinData outputAddress
        (2, [outputAddress]) ->
            Close <$> PTx.fromBuiltinData outputAddress
        _ -> Nothing

instance PTx.ToData AccountContent where
    toBuiltinData AccountContent{acOwner, acAsset, acSnapshot, acRequest} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkB acOwner
            , PlutusTx.mkB acAsset
            , PTx.toBuiltinData acSnapshot
            , maybeToOptionData acRequest
            ]

instance PTx.FromData AccountContent where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just acOwner, D.getB -> Just acAsset, snapshot, request]) ->
            AccountContent
                <$> pure acOwner
                <*> pure acAsset
                <*> PTx.fromBuiltinData snapshot
                <*> maybeFromOptionData PTx.fromBuiltinData request
        _ -> Nothing

instance PTx.ToData SnapshotEpochToScaleToSumContent where
    toBuiltinData SnapshotEpochToScaleToSumContent{sesAsset, sesSnapshot} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkB sesAsset
            , PlutusTx.mkMap
                [ (PTx.toBuiltinData key, PTx.toBuiltinData value)
                | (key, value) <- Map.toAscList sesSnapshot
                ]
            ]

instance PTx.FromData SnapshotEpochToScaleToSumContent where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just sesAsset, snapshot]) -> do
            entries <- fromMap snapshot
            decodedEntries <-
                traverse
                    (\(keyData, valueData) -> do
                        key <- PTx.fromBuiltinData keyData
                        value <- PTx.fromBuiltinData valueData
                        pure (key, value)
                    )
                    entries
            SnapshotEpochToScaleToSumContent
                <$> pure sesAsset
                <*> pure (Map.fromList decodedEntries)
        _ -> Nothing

instance PTx.ToData StabilityPoolDatum where
    toBuiltinData = \case
        StabilityPool spdContent ->
            PlutusTx.mkConstr 0 [stabilityPoolContentToVariantData spdContent]
        Account spdAccountContent ->
            PlutusTx.mkConstr 1 [accountContentToVariantData spdAccountContent]
        SnapshotEpochToScaleToSum spdSnapshotContent ->
            PlutusTx.mkConstr 2 [snapshotContentToVariantData spdSnapshotContent]

instance PTx.FromData StabilityPoolDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [content]) ->
            StabilityPool <$> stabilityPoolContentFromVariantData content
        (1, [content]) ->
            Account <$> accountContentFromVariantData content
        (2, [content]) ->
            SnapshotEpochToScaleToSum <$> snapshotContentFromVariantData content
        _ -> Nothing

stabilityPoolContentToVariantData :: StabilityPoolContent -> PlutusTx.BuiltinData
stabilityPoolContentToVariantData StabilityPoolContent{spcAsset, spcSnapshot, spcEpochToScaleToSum} =
    PlutusTx.mkConstr
        0
        [ PlutusTx.mkB spcAsset
        , PTx.toBuiltinData spcSnapshot
        , PlutusTx.mkMap
            [ (PTx.toBuiltinData key, PTx.toBuiltinData value)
            | (key, value) <- Map.toAscList spcEpochToScaleToSum
            ]
        ]

stabilityPoolContentFromVariantData :: PlutusTx.BuiltinData -> Maybe StabilityPoolContent
stabilityPoolContentFromVariantData dt = D.withConstr dt $ \case
    (0, [D.getB -> Just spcAsset, snapshot, epochToScaleToSum]) -> do
        entries <- fromMap epochToScaleToSum
        decodedEntries <-
            traverse
                (\(keyData, valueData) -> do
                    key <- PTx.fromBuiltinData keyData
                    value <- PTx.fromBuiltinData valueData
                    pure (key, value)
                )
                entries
        StabilityPoolContent
            <$> pure spcAsset
            <*> PTx.fromBuiltinData snapshot
            <*> pure (Map.fromList decodedEntries)
    _ -> Nothing

accountContentToVariantData :: AccountContent -> PlutusTx.BuiltinData
accountContentToVariantData AccountContent{acOwner, acAsset, acSnapshot, acRequest} =
    PlutusTx.mkConstr
        0
        [ PlutusTx.mkB acOwner
        , PlutusTx.mkB acAsset
        , PTx.toBuiltinData acSnapshot
        , maybeToOptionData acRequest
        ]

accountContentFromVariantData :: PlutusTx.BuiltinData -> Maybe AccountContent
accountContentFromVariantData dt = D.withConstr dt $ \case
    (0, [D.getB -> Just acOwner, D.getB -> Just acAsset, snapshot, request]) ->
        AccountContent
            <$> pure acOwner
            <*> pure acAsset
            <*> PTx.fromBuiltinData snapshot
            <*> maybeFromOptionData PTx.fromBuiltinData request
    _ -> Nothing

snapshotContentToVariantData :: SnapshotEpochToScaleToSumContent -> PlutusTx.BuiltinData
snapshotContentToVariantData SnapshotEpochToScaleToSumContent{sesAsset, sesSnapshot} =
    PlutusTx.mkConstr
        0
        [ PlutusTx.mkB sesAsset
        , PlutusTx.mkMap
            [ (PTx.toBuiltinData key, PTx.toBuiltinData value)
            | (key, value) <- Map.toAscList sesSnapshot
            ]
        ]

snapshotContentFromVariantData :: PlutusTx.BuiltinData -> Maybe SnapshotEpochToScaleToSumContent
snapshotContentFromVariantData dt = D.withConstr dt $ \case
    (0, [D.getB -> Just sesAsset, snapshot]) -> do
        entries <- fromMap snapshot
        decodedEntries <-
            traverse
                (\(keyData, valueData) -> do
                    key <- PTx.fromBuiltinData keyData
                    value <- PTx.fromBuiltinData valueData
                    pure (key, value)
                )
                entries
        SnapshotEpochToScaleToSumContent
            <$> pure sesAsset
            <*> pure (Map.fromList decodedEntries)
    _ -> Nothing
