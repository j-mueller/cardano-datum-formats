{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.MinSwap.Order (
    MinSwapV2Order (..),
    ExpirySetting (..),
    Step (..),
    classifyStep,
    stepBatcherFee,
    stepMetadataText,
    AuthorizationMethod (..),
    ExtraDatum (..),
    Direction (..),
    invert,
    direction,
    Killable (..),
    DepositAmount (..),
    SwapAmount (..),
    WithdrawAmount (..),
    Route (..),
) where

import Cardano.Api qualified as C
import Cardano.Asset qualified as Asset
import Cardano.Asset.Pair (Pair)
import Cardano.Asset.Pair qualified as Pair
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (
    jsonOptions,
    stripFieldPrefix,
    stripFieldPrefixes,
    sumOptions,
 )
import Cardano.Protocol.JSON ()
import Control.Monad ((>=>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (TypeScript (..), deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import Data.Text (Text)
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Hedgehog qualified as H

data AuthorizationMethod
    = Signature (C.Hash C.PaymentKey)
    | SpendScript C.ScriptHash
    | WithdrawScript C.ScriptHash
    | MintScript C.ScriptHash
    deriving stock (Eq, Ord, Show, Generic)

instance PlutusTx.ToData AuthorizationMethod where
    toBuiltinData = \case
        Signature hsh -> PlutusTx.mkConstr 0 [D.serialiseHash hsh]
        SpendScript hsh -> PlutusTx.mkConstr 1 [D.serialiseScriptHash hsh]
        WithdrawScript hsh -> PlutusTx.mkConstr 2 [D.serialiseScriptHash hsh]
        MintScript hsh -> PlutusTx.mkConstr 3 [D.serialiseScriptHash hsh]

instance PlutusTx.FromData AuthorizationMethod where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [D.getB >=> D.deserialiseHash -> Just bs]) -> Just (Signature bs)
        (1, [D.getB >=> D.deserialiseScriptHash -> Just bs]) -> Just (SpendScript bs)
        (2, [D.getB >=> D.deserialiseScriptHash -> Just bs]) -> Just (WithdrawScript bs)
        (3, [D.getB >=> D.deserialiseScriptHash -> Just bs]) -> Just (MintScript bs)
        _ -> Nothing

instance Arbitrary AuthorizationMethod where
    arbitrary =
        Gen.oneof
            [ Signature <$> H.hedgehog (Gen.genVerificationKeyHash C.AsPaymentKey)
            , SpendScript <$> H.hedgehog Gen.genScriptHash
            , WithdrawScript <$> H.hedgehog Gen.genScriptHash
            , MintScript <$> H.hedgehog Gen.genScriptHash
            ]

data ExtraDatum
    = NoDatum
    | DatumHash (C.Hash C.ScriptData)
    | InlineDatum (C.Hash C.ScriptData)
    deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary ExtraDatum where
    arbitrary =
        Gen.oneof
            [ pure NoDatum
            , DatumHash <$> H.hedgehog Gen.genHashScriptData
            , InlineDatum <$> H.hedgehog Gen.genHashScriptData
            ]

instance PlutusTx.ToData ExtraDatum where
    toBuiltinData = \case
        NoDatum -> PlutusTx.mkConstr 0 []
        DatumHash hsh -> PlutusTx.mkConstr 1 [D.serialiseHash hsh]
        InlineDatum hsh -> PlutusTx.mkConstr 2 [D.serialiseHash hsh]

instance PlutusTx.FromData ExtraDatum where
    fromBuiltinData k =
        D.withConstr k $ \case
            (0, []) -> Just NoDatum
            (1, [D.getB >=> D.deserialiseHash -> Just bs]) -> Just $ DatumHash bs
            (2, [D.getB >=> D.deserialiseHash -> Just bs]) -> Just $ InlineDatum bs
            _ -> Nothing

data Direction
    = BtoA
    | AtoB
    deriving stock (Eq, Ord, Show, Enum, Generic)

invert :: Direction -> Direction
invert BtoA = AtoB
invert AtoB = BtoA

direction :: C.AssetId -> Pair -> Maybe (Direction, C.AssetId)
direction assetIn pair'
    | Pair.asset1 pair' == assetIn = Just (AtoB, Pair.asset2 pair')
    | Pair.asset2 pair' == assetIn = Just (BtoA, Pair.asset1 pair')
    | otherwise = Nothing

instance Arbitrary Direction where
    arbitrary = Gen.elements [BtoA, AtoB]

instance PlutusTx.ToData Direction where
    toBuiltinData = \case
        BtoA -> PlutusTx.mkConstr 0 []
        AtoB -> PlutusTx.mkConstr 1 []

instance PlutusTx.FromData Direction where
    fromBuiltinData k = D.withConstr k $ \case
        (0, []) -> Just BtoA
        (1, []) -> Just AtoB
        _ -> Nothing

data Killable
    = PendingOnFailed
    | KillOnFailed
    deriving stock (Eq, Ord, Show, Enum, Generic)

instance Arbitrary Killable where
    arbitrary = Gen.elements [PendingOnFailed, KillOnFailed]

instance PlutusTx.ToData Killable where
    toBuiltinData = \case
        PendingOnFailed -> PlutusTx.mkConstr 0 []
        KillOnFailed -> PlutusTx.mkConstr 1 []

instance PlutusTx.FromData Killable where
    fromBuiltinData k = D.withConstr k $ \case
        (0, []) -> Just PendingOnFailed
        (1, []) -> Just KillOnFailed
        _ -> Nothing

data DepositAmount
    = DepositSpecificAmount {depositAmountA :: C.Quantity, depositAmountB :: C.Quantity}
    | DepositAll {deductedAmountA :: C.Quantity, deductedAmountB :: C.Quantity}
    deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary DepositAmount where
    arbitrary =
        let q = H.hedgehog Gen.genSignedNonZeroQuantity
         in Gen.oneof
                [ DepositSpecificAmount <$> q <*> q
                , DepositAll <$> q <*> q
                ]

instance PlutusTx.ToData DepositAmount where
    toBuiltinData = \case
        DepositSpecificAmount (C.Quantity q1) (C.Quantity q2) -> PlutusTx.mkConstr 0 [PlutusTx.mkI q1, PlutusTx.mkI q2]
        DepositAll (C.Quantity q1) (C.Quantity q2) -> PlutusTx.mkConstr 1 [PlutusTx.mkI q1, PlutusTx.mkI q2]

instance PlutusTx.FromData DepositAmount where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [fmap C.Quantity . D.getI -> Just q1, fmap C.Quantity . D.getI -> Just q2]) -> Just (DepositSpecificAmount q1 q2)
        (1, [fmap C.Quantity . D.getI -> Just q1, fmap C.Quantity . D.getI -> Just q2]) -> Just (DepositAll q1 q2)
        _ -> Nothing

data SwapAmount
    = SwapSpecificAmount {swapAmount :: C.Quantity}
    | SwapAll {deductedSwapAmount :: C.Quantity}
    deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary SwapAmount where
    arbitrary =
        let q = H.hedgehog Gen.genSignedNonZeroQuantity
         in Gen.oneof
                [ SwapSpecificAmount <$> q
                , SwapAll <$> q
                ]

instance PlutusTx.ToData SwapAmount where
    toBuiltinData = \case
        SwapSpecificAmount (C.Quantity q1) -> PlutusTx.mkConstr 0 [PlutusTx.mkI q1]
        SwapAll (C.Quantity q1) -> PlutusTx.mkConstr 1 [PlutusTx.mkI q1]

instance PlutusTx.FromData SwapAmount where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [fmap C.Quantity . D.getI -> Just q1]) -> Just (SwapSpecificAmount q1)
        (1, [fmap C.Quantity . D.getI -> Just q1]) -> Just (SwapAll q1)
        _ -> Nothing

data WithdrawAmount
    = WithdrawAmount {withdrawalLPAmount :: C.Quantity}
    | WithdrawAll {deductedLPAmount :: C.Quantity}
    deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary WithdrawAmount where
    arbitrary =
        let q = H.hedgehog Gen.genSignedNonZeroQuantity
         in Gen.oneof
                [ WithdrawAmount <$> q
                , WithdrawAll <$> q
                ]

instance PlutusTx.ToData WithdrawAmount where
    toBuiltinData = \case
        WithdrawAmount (C.Quantity q1) -> PlutusTx.mkConstr 0 [PlutusTx.mkI q1]
        WithdrawAll (C.Quantity q1) -> PlutusTx.mkConstr 1 [PlutusTx.mkI q1]

instance PlutusTx.FromData WithdrawAmount where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [fmap C.Quantity . D.getI -> Just q1]) -> Just (WithdrawAmount q1)
        (1, [fmap C.Quantity . D.getI -> Just q1]) -> Just (WithdrawAll q1)
        _ -> Nothing

data Route = Route
    { rLpAsset :: C.AssetId
    , rDirection :: Direction
    }
    deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary Route where
    arbitrary =
        Route <$> H.hedgehog Gen.genAssetId <*> arbitrary

instance PlutusTx.ToData Route where
    toBuiltinData Route{rLpAsset, rDirection} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.toBuiltinData $ Asset.fromAssetId rLpAsset
            , PlutusTx.toBuiltinData rDirection
            ]

instance PlutusTx.FromData Route where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [PlutusTx.fromBuiltinData >=> Asset.toAssetId -> Just rLpAsset, PlutusTx.fromBuiltinData -> Just rDirection]) ->
            Just Route{rLpAsset, rDirection}
        _ -> Nothing

data Step
    = SwapExactIn Direction SwapAmount C.Quantity Killable
    | Stop Direction SwapAmount C.Quantity
    | OCO Direction SwapAmount C.Quantity C.Quantity
    | SwapExactOut Direction SwapAmount C.Quantity Killable
    | Deposit DepositAmount C.Quantity Killable
    | Withdraw WithdrawAmount C.Quantity C.Quantity Killable
    | ZapOut Direction WithdrawAmount C.Quantity Killable
    | PartialSwap Direction C.Quantity C.Quantity C.Quantity C.Quantity C.Quantity C.Quantity
    | WithdrawImbalance WithdrawAmount C.Quantity C.Quantity C.Quantity Killable
    | SwapRouting [Route] SwapAmount C.Quantity
    | Donation
    deriving stock (Eq, Ord, Show, Generic)

classifyStep :: Step -> Text
classifyStep = \case
    SwapExactIn{} -> "SwapExactIn"
    Stop{} -> "Stop"
    OCO{} -> "OCO"
    SwapExactOut{} -> "SwapExactOut"
    Deposit{} -> "Deposit"
    Withdraw{} -> "Withdraw"
    ZapOut{} -> "ZapOut"
    PartialSwap{} -> "PartialSwap"
    WithdrawImbalance{} -> "WithdrawImbalance"
    SwapRouting{} -> "SwapRouting"
    Donation{} -> "Donation"

stepBatcherFee :: Step -> C.Quantity
stepBatcherFee = \case
    SwapExactIn{} -> C.Quantity 700_000
    Stop{} -> C.Quantity 700_000
    OCO{} -> C.Quantity 700_000
    SwapExactOut{} -> C.Quantity 700_000
    Deposit{} -> C.Quantity 750_000
    Withdraw{} -> C.Quantity 700_000
    ZapOut{} -> C.Quantity 700_000
    PartialSwap{} -> C.Quantity 720_000
    WithdrawImbalance{} -> C.Quantity 750_000
    SwapRouting{} -> C.Quantity 900_000
    Donation{} -> C.Quantity 700_000

stepMetadataText :: Step -> Text
stepMetadataText = \case
    SwapExactIn{} -> "Minswap: Market Order"
    Stop{} -> "SDK Minswap: Stop Order"
    OCO{} -> "SDK Minswap: OCO Order"
    SwapExactOut{} -> "Minswap: Market Order"
    Deposit{} -> "SDK Minswap: Deposit Order"
    Withdraw{} -> "SDK Minswap: Withdraw Order"
    ZapOut{} -> "SDK Minswap: Zap Out Order"
    PartialSwap{} -> "SDK Minswap: Partial Fill Order"
    WithdrawImbalance{} -> "WithdrawImbalance"
    SwapRouting{} -> "SDK Minswap: Routing Order"
    Donation{} -> "SDK Minswap: Donation Order"

instance Arbitrary Step where
    arbitrary =
        Gen.oneof
            [ SwapExactIn <$> arbitrary <*> arbitrary <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> arbitrary
            , Stop <$> arbitrary <*> arbitrary <*> H.hedgehog Gen.genSignedNonZeroQuantity
            , OCO <$> arbitrary <*> arbitrary <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> H.hedgehog Gen.genSignedNonZeroQuantity
            , SwapExactOut <$> arbitrary <*> arbitrary <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> arbitrary
            , Deposit <$> arbitrary <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> arbitrary
            , Withdraw <$> arbitrary <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> arbitrary
            , ZapOut <$> arbitrary <*> arbitrary <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> arbitrary
            , PartialSwap <$> arbitrary <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> H.hedgehog Gen.genSignedNonZeroQuantity
            , WithdrawImbalance <$> arbitrary <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> H.hedgehog Gen.genSignedNonZeroQuantity <*> arbitrary
            , SwapRouting <$> arbitrary <*> arbitrary <*> H.hedgehog Gen.genSignedNonZeroQuantity
            , pure Donation
            ]

instance PlutusTx.ToData Step where
    toBuiltinData = \case
        SwapExactIn dir amt (C.Quantity minRec) killable ->
            PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData dir, PlutusTx.toBuiltinData amt, PlutusTx.toBuiltinData minRec, PlutusTx.toBuiltinData killable]
        Stop dir amt (C.Quantity received) ->
            PlutusTx.mkConstr 1 [PlutusTx.toBuiltinData dir, PlutusTx.toBuiltinData amt, PlutusTx.toBuiltinData received]
        OCO dir amt (C.Quantity minReceived) (C.Quantity stopReceived) ->
            PlutusTx.mkConstr 2 [PlutusTx.toBuiltinData dir, PlutusTx.toBuiltinData amt, PlutusTx.toBuiltinData minReceived, PlutusTx.toBuiltinData stopReceived]
        SwapExactOut dir amt (C.Quantity minReceived) killable ->
            PlutusTx.mkConstr 3 [PlutusTx.toBuiltinData dir, PlutusTx.toBuiltinData amt, PlutusTx.toBuiltinData minReceived, PlutusTx.toBuiltinData killable]
        Deposit amt (C.Quantity minReceived) killable ->
            PlutusTx.mkConstr 4 [PlutusTx.toBuiltinData amt, PlutusTx.toBuiltinData minReceived, PlutusTx.toBuiltinData killable]
        Withdraw amt (C.Quantity minReceivedA) (C.Quantity minReceivedB) killable ->
            PlutusTx.mkConstr 5 [PlutusTx.toBuiltinData amt, PlutusTx.toBuiltinData minReceivedA, PlutusTx.toBuiltinData minReceivedB, PlutusTx.toBuiltinData killable]
        ZapOut dir amt (C.Quantity zapOutAmount) killable ->
            PlutusTx.mkConstr 6 [PlutusTx.toBuiltinData dir, PlutusTx.toBuiltinData amt, PlutusTx.toBuiltinData zapOutAmount, PlutusTx.toBuiltinData killable]
        PartialSwap dir (C.Quantity totalSwapAmount) (C.Quantity ioRatioNumerator) (C.Quantity ioRatioDenominator) (C.Quantity hops) (C.Quantity minimumSwapAmountRequired) (C.Quantity maxBatcherFeeEachTime) ->
            PlutusTx.mkConstr 7 [PlutusTx.toBuiltinData dir, PlutusTx.toBuiltinData totalSwapAmount, PlutusTx.toBuiltinData ioRatioNumerator, PlutusTx.toBuiltinData ioRatioDenominator, PlutusTx.toBuiltinData hops, PlutusTx.toBuiltinData minimumSwapAmountRequired, PlutusTx.toBuiltinData maxBatcherFeeEachTime]
        WithdrawImbalance amt (C.Quantity ratioAssetA) (C.Quantity ratioAssetB) (C.Quantity minimumAssetA) killable ->
            PlutusTx.mkConstr 8 [PlutusTx.toBuiltinData amt, PlutusTx.toBuiltinData ratioAssetA, PlutusTx.toBuiltinData ratioAssetB, PlutusTx.toBuiltinData minimumAssetA, PlutusTx.toBuiltinData killable]
        SwapRouting routes swapAmount (C.Quantity minReceived) ->
            PlutusTx.mkConstr 9 [PlutusTx.toBuiltinData routes, PlutusTx.toBuiltinData swapAmount, PlutusTx.toBuiltinData minReceived]
        Donation -> PlutusTx.mkConstr 10 []

instance PlutusTx.FromData Step where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [dir, amt, minRec, killable]) ->
            SwapExactIn <$> PlutusTx.fromBuiltinData dir <*> PlutusTx.fromBuiltinData amt <*> fmap C.Quantity (PlutusTx.fromBuiltinData minRec) <*> PlutusTx.fromBuiltinData killable
        (1, [dir, received, amt]) ->
            Stop <$> PlutusTx.fromBuiltinData dir <*> PlutusTx.fromBuiltinData received <*> fmap C.Quantity (PlutusTx.fromBuiltinData amt)
        (2, [dir, amt, minReceived, stopReceived]) ->
            OCO <$> PlutusTx.fromBuiltinData dir <*> PlutusTx.fromBuiltinData amt <*> fmap C.Quantity (PlutusTx.fromBuiltinData minReceived) <*> fmap C.Quantity (PlutusTx.fromBuiltinData stopReceived)
        (3, [dir, amt, minRec, killable]) ->
            SwapExactOut <$> PlutusTx.fromBuiltinData dir <*> PlutusTx.fromBuiltinData amt <*> fmap C.Quantity (PlutusTx.fromBuiltinData minRec) <*> PlutusTx.fromBuiltinData killable
        (4, [amt, minRec, killable]) ->
            Deposit <$> PlutusTx.fromBuiltinData amt <*> fmap C.Quantity (PlutusTx.fromBuiltinData minRec) <*> PlutusTx.fromBuiltinData killable
        (5, [amt, minRecA, minRecB, killable]) ->
            Withdraw <$> PlutusTx.fromBuiltinData amt <*> fmap C.Quantity (PlutusTx.fromBuiltinData minRecA) <*> fmap C.Quantity (PlutusTx.fromBuiltinData minRecB) <*> PlutusTx.fromBuiltinData killable
        (6, [dir, amt, minReceived, killable]) ->
            ZapOut <$> PlutusTx.fromBuiltinData dir <*> PlutusTx.fromBuiltinData amt <*> fmap C.Quantity (PlutusTx.fromBuiltinData minReceived) <*> PlutusTx.fromBuiltinData killable
        (7, [dir, totalSwapAmount, ioRatioNumerator, ioRatioDenominator, hops, minimumSwapAmountRequired, maxBatcherFeeEachTime]) ->
            PartialSwap <$> PlutusTx.fromBuiltinData dir <*> fmap C.Quantity (PlutusTx.fromBuiltinData totalSwapAmount) <*> fmap C.Quantity (PlutusTx.fromBuiltinData ioRatioNumerator) <*> fmap C.Quantity (PlutusTx.fromBuiltinData ioRatioDenominator) <*> fmap C.Quantity (PlutusTx.fromBuiltinData hops) <*> fmap C.Quantity (PlutusTx.fromBuiltinData minimumSwapAmountRequired) <*> fmap C.Quantity (PlutusTx.fromBuiltinData maxBatcherFeeEachTime)
        (8, [amt, ratioAssetA, ratioAssetB, minimumAssetA, killable]) ->
            WithdrawImbalance <$> PlutusTx.fromBuiltinData amt <*> fmap C.Quantity (PlutusTx.fromBuiltinData ratioAssetA) <*> fmap C.Quantity (PlutusTx.fromBuiltinData ratioAssetB) <*> fmap C.Quantity (PlutusTx.fromBuiltinData minimumAssetA) <*> PlutusTx.fromBuiltinData killable
        (9, [routes, swapAmount, minReceived]) ->
            SwapRouting <$> PlutusTx.fromBuiltinData routes <*> PlutusTx.fromBuiltinData swapAmount <*> fmap C.Quantity (PlutusTx.fromBuiltinData minReceived)
        (10, []) -> Just Donation
        _ -> Nothing

data ExpirySetting = ExpirySetting
    { esExpiredTime :: !Integer
    , esMaxCancellationTip :: C.Quantity
    }
    deriving stock (Eq, Ord, Show, Generic)

instance Arbitrary ExpirySetting where
    arbitrary = ExpirySetting <$> arbitrary <*> H.hedgehog Gen.genSignedNonZeroQuantity

instance PlutusTx.ToData ExpirySetting where
    toBuiltinData = \case
        ExpirySetting expTime (C.Quantity tip) ->
            PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData expTime, PlutusTx.toBuiltinData tip]

instance PlutusTx.FromData ExpirySetting where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [expTime, tip]) -> ExpirySetting <$> PlutusTx.fromBuiltinData expTime <*> fmap C.Quantity (PlutusTx.fromBuiltinData tip)
        _ -> Nothing

data MinSwapV2Order addr = MinSwapV2Order
    { msCanceller :: AuthorizationMethod
    , msRefundReceiver :: addr
    , msRefundReceiverDatum :: ExtraDatum
    , msSuccessReceiver :: addr
    , msSuccessReceiverDatum :: ExtraDatum
    , msLpAsset :: C.AssetId
    , msStep :: Step
    , msMaxBatcherFee :: C.Quantity
    , msExpiredOptions :: Maybe ExpirySetting
    }
    deriving stock (Eq, Ord, Show, Functor, Generic)

instance Arbitrary addr => Arbitrary (MinSwapV2Order addr) where
    arbitrary =
        MinSwapV2Order
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> H.hedgehog Gen.genAssetId
            <*> arbitrary
            <*> H.hedgehog Gen.genPositiveQuantity
            <*> arbitrary

instance PlutusTx.ToData addr => PlutusTx.ToData (MinSwapV2Order addr) where
    toBuiltinData order =
        PlutusTx.mkConstr
            0
            [ PlutusTx.toBuiltinData (msCanceller order)
            , PlutusTx.toBuiltinData (msRefundReceiver order)
            , PlutusTx.toBuiltinData (msRefundReceiverDatum order)
            , PlutusTx.toBuiltinData (msSuccessReceiver order)
            , PlutusTx.toBuiltinData (msSuccessReceiverDatum order)
            , PlutusTx.toBuiltinData (Asset.fromAssetId $ msLpAsset order)
            , PlutusTx.toBuiltinData (msStep order)
            , let C.Quantity fee = msMaxBatcherFee order in PlutusTx.toBuiltinData fee
            , PlutusTx.toBuiltinData (msExpiredOptions order)
            ]

instance PlutusTx.FromData addr => PlutusTx.FromData (MinSwapV2Order addr) where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [canceller, refundReceiver, refundReceiverDatum, successReceiver, successReceiverDatum, lpAsset, step, maxBatcherFee, expiredOptions]) ->
            MinSwapV2Order
                <$> PlutusTx.fromBuiltinData canceller
                <*> PlutusTx.fromBuiltinData refundReceiver
                <*> PlutusTx.fromBuiltinData refundReceiverDatum
                <*> PlutusTx.fromBuiltinData successReceiver
                <*> PlutusTx.fromBuiltinData successReceiverDatum
                <*> (PlutusTx.fromBuiltinData >=> Asset.toAssetId) lpAsset
                <*> PlutusTx.fromBuiltinData step
                <*> fmap C.Quantity (PlutusTx.fromBuiltinData maxBatcherFee)
                <*> PlutusTx.fromBuiltinData expiredOptions
        _ -> Nothing

instance ToJSON AuthorizationMethod where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON AuthorizationMethod where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

$(deriveTypeScript (sumOptions 0) ''AuthorizationMethod)

instance Schema.ToSchema AuthorizationMethod where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

instance ToJSON ExtraDatum where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON ExtraDatum where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

$(deriveTypeScript (sumOptions 0) ''ExtraDatum)

instance Schema.ToSchema ExtraDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

directionOptions :: Aeson.Options
directionOptions =
    (sumOptions 0)
        { Aeson.constructorTagModifier = \case
            "BtoA" -> "b_to_a"
            "AtoB" -> "a_to_b"
            other -> Aeson.camelTo2 '_' other
        }

instance ToJSON Direction where
    toJSON = Aeson.genericToJSON directionOptions
    toEncoding = Aeson.genericToEncoding directionOptions

instance FromJSON Direction where
    parseJSON = Aeson.genericParseJSON directionOptions

$(deriveTypeScript ((sumOptions 0){Aeson.constructorTagModifier = \case
    "BtoA" -> "b_to_a"
    "AtoB" -> "a_to_b"
    other -> Aeson.camelTo2 '_' other}) ''Direction)

instance Schema.ToSchema Direction where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions directionOptions)

instance ToJSON Killable where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON Killable where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

$(deriveTypeScript (sumOptions 0) ''Killable)

instance Schema.ToSchema Killable where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

depositAmountOptions :: Aeson.Options
depositAmountOptions =
    (sumOptions 0)
        { Aeson.fieldLabelModifier = stripFieldPrefixes ["depositAmount", "deductedAmount"]
        }

instance ToJSON DepositAmount where
    toJSON = Aeson.genericToJSON depositAmountOptions
    toEncoding = Aeson.genericToEncoding depositAmountOptions

instance FromJSON DepositAmount where
    parseJSON = Aeson.genericParseJSON depositAmountOptions

$(deriveTypeScript ((sumOptions 0){Aeson.fieldLabelModifier = stripFieldPrefixes ["depositAmount", "deductedAmount"]}) ''DepositAmount)

instance Schema.ToSchema DepositAmount where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions depositAmountOptions)

swapAmountOptions :: Aeson.Options
swapAmountOptions =
    (sumOptions 0)
        { Aeson.fieldLabelModifier = stripFieldPrefixes ["swap", "deductedSwap"]
        }

instance ToJSON SwapAmount where
    toJSON = Aeson.genericToJSON swapAmountOptions
    toEncoding = Aeson.genericToEncoding swapAmountOptions

instance FromJSON SwapAmount where
    parseJSON = Aeson.genericParseJSON swapAmountOptions

$(deriveTypeScript ((sumOptions 0){Aeson.fieldLabelModifier = stripFieldPrefixes ["swap", "deductedSwap"]}) ''SwapAmount)

instance Schema.ToSchema SwapAmount where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions swapAmountOptions)

withdrawAmountOptions :: Aeson.Options
withdrawAmountOptions =
    (sumOptions 0)
        { Aeson.fieldLabelModifier = stripFieldPrefixes ["withdrawalLP", "deductedLP"]
        }

instance ToJSON WithdrawAmount where
    toJSON = Aeson.genericToJSON withdrawAmountOptions
    toEncoding = Aeson.genericToEncoding withdrawAmountOptions

instance FromJSON WithdrawAmount where
    parseJSON = Aeson.genericParseJSON withdrawAmountOptions

$(deriveTypeScript ((sumOptions 0){Aeson.fieldLabelModifier = stripFieldPrefixes ["withdrawalLP", "deductedLP"]}) ''WithdrawAmount)

instance Schema.ToSchema WithdrawAmount where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions withdrawAmountOptions)

instance ToJSON Route where
    toJSON = Aeson.genericToJSON (jsonOptions 1)
    toEncoding = Aeson.genericToEncoding (jsonOptions 1)

instance FromJSON Route where
    parseJSON = Aeson.genericParseJSON (jsonOptions 1)

$(deriveTypeScript (jsonOptions 1) ''Route)

instance Schema.ToSchema Route where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 1))

instance ToJSON Step where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON Step where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

instance TypeScript Step where
    getTypeScriptType _ = "any"

instance Schema.ToSchema Step where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

instance ToJSON ExpirySetting where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON ExpirySetting where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''ExpirySetting)

instance Schema.ToSchema ExpirySetting where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSON addr => ToJSON (MinSwapV2Order addr) where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON addr => FromJSON (MinSwapV2Order addr) where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''MinSwapV2Order)

instance Schema.ToSchema addr => Schema.ToSchema (MinSwapV2Order addr) where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
