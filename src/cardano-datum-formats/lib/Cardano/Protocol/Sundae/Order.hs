module Cardano.Protocol.Sundae.Order (
    SundaeOrderDatum (..),
    MultisigScript (..),
    Credential (..),
    StakingCredential (..),
    SundaeAddress (..),
    SundaeDatum (..),
    Destination (..),
    StrategyAuthorization (..),
    AssetAmount (..),
    Order (..),
) where

import Cardano.Api qualified as C
import Cardano.Data qualified as D
import Control.Monad ((>=>))
import Data.ByteString qualified as BS
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Hedgehog qualified as H

data MultisigScript
    = Signature (C.Hash C.PaymentKey)
    | AllOf [MultisigScript]
    | AnyOf [MultisigScript]
    | AtLeast Integer [MultisigScript]
    | Before Integer
    | After Integer
    | Script C.ScriptHash
    deriving stock (Eq, Show)

instance Arbitrary MultisigScript where
    arbitrary = QC.sized go
      where
        go :: Int -> Gen.Gen MultisigScript
        go n
            | n <= 1 =
                Gen.oneof
                    [ Signature <$> H.hedgehog (Gen.genVerificationKeyHash C.AsPaymentKey)
                    , Script <$> H.hedgehog Gen.genScriptHash
                    , Before <$> arbitrary
                    , After <$> arbitrary
                    ]
            | otherwise =
                Gen.frequency
                    [ (4, go 1)
                    , (1, AllOf <$> Gen.resize (n `div` 2) arbitrary)
                    , (1, AnyOf <$> Gen.resize (n `div` 2) arbitrary)
                    , (1, AtLeast <$> arbitrary <*> Gen.resize (n `div` 2) arbitrary)
                    ]

instance PlutusTx.ToData MultisigScript where
    toBuiltinData = \case
        Signature keyHash -> PlutusTx.mkConstr 0 [D.serialiseHash keyHash]
        AllOf scripts -> PlutusTx.mkConstr 1 [PlutusTx.toBuiltinData scripts]
        AnyOf scripts -> PlutusTx.mkConstr 2 [PlutusTx.toBuiltinData scripts]
        AtLeast required scripts ->
            PlutusTx.mkConstr
                3
                [ PlutusTx.mkI required
                , PlutusTx.toBuiltinData scripts
                ]
        Before time -> PlutusTx.mkConstr 4 [PlutusTx.mkI time]
        After time -> PlutusTx.mkConstr 5 [PlutusTx.mkI time]
        Script scriptHash -> PlutusTx.mkConstr 6 [D.serialiseScriptHash scriptHash]

instance PlutusTx.FromData MultisigScript where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [D.getB >=> D.deserialiseHash -> Just keyHash]) -> Just (Signature keyHash)
        (1, [scripts]) -> AllOf <$> PlutusTx.fromBuiltinData scripts
        (2, [scripts]) -> AnyOf <$> PlutusTx.fromBuiltinData scripts
        (3, [D.getI -> Just required, scripts]) ->
            AtLeast required <$> PlutusTx.fromBuiltinData scripts
        (4, [D.getI -> Just time]) -> Just (Before time)
        (5, [D.getI -> Just time]) -> Just (After time)
        (6, [D.getB >=> D.deserialiseScriptHash -> Just scriptHash]) -> Just (Script scriptHash)
        _ -> Nothing

data Credential
    = VerificationKeyCredential (C.Hash C.PaymentKey)
    | ScriptCredential C.ScriptHash
    deriving stock (Eq, Show)

instance Arbitrary Credential where
    arbitrary =
        Gen.oneof
            [ VerificationKeyCredential <$> H.hedgehog (Gen.genVerificationKeyHash C.AsPaymentKey)
            , ScriptCredential <$> H.hedgehog Gen.genScriptHash
            ]

instance PlutusTx.ToData Credential where
    toBuiltinData = \case
        VerificationKeyCredential keyHash -> PlutusTx.mkConstr 0 [D.serialiseHash keyHash]
        ScriptCredential scriptHash -> PlutusTx.mkConstr 1 [D.serialiseScriptHash scriptHash]

instance PlutusTx.FromData Credential where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [D.getB >=> D.deserialiseHash -> Just keyHash]) -> Just (VerificationKeyCredential keyHash)
        (1, [D.getB >=> D.deserialiseScriptHash -> Just scriptHash]) -> Just (ScriptCredential scriptHash)
        _ -> Nothing

data StakingCredential
    = Inline Credential
    | Pointer
        { pointerSlotNumber :: Integer
        , pointerTransactionIndex :: Integer
        , pointerCertificateIndex :: Integer
        }
    deriving stock (Eq, Show)

instance Arbitrary StakingCredential where
    arbitrary =
        Gen.oneof
            [ Inline <$> arbitrary
            , Pointer <$> arbitrary <*> arbitrary <*> arbitrary
            ]

instance PlutusTx.ToData StakingCredential where
    toBuiltinData = \case
        Inline cred -> PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData cred]
        Pointer pointerSlotNumber pointerTransactionIndex pointerCertificateIndex ->
            PlutusTx.mkConstr
                1
                [ PlutusTx.mkI pointerSlotNumber
                , PlutusTx.mkI pointerTransactionIndex
                , PlutusTx.mkI pointerCertificateIndex
                ]

instance PlutusTx.FromData StakingCredential where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [cred]) -> Inline <$> PlutusTx.fromBuiltinData cred
        (1, [D.getI -> Just pointerSlotNumber, D.getI -> Just pointerTransactionIndex, D.getI -> Just pointerCertificateIndex]) ->
            Just Pointer{pointerSlotNumber, pointerTransactionIndex, pointerCertificateIndex}
        _ -> Nothing

data SundaeAddress = SundaeAddress
    { saPaymentCredential :: Credential
    , saStakeCredential :: Maybe StakingCredential
    }
    deriving stock (Eq, Show)

instance Arbitrary SundaeAddress where
    arbitrary =
        SundaeAddress <$> arbitrary <*> arbitrary

instance PlutusTx.ToData SundaeAddress where
    toBuiltinData SundaeAddress{saPaymentCredential, saStakeCredential} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.toBuiltinData saPaymentCredential
            , PlutusTx.toBuiltinData saStakeCredential
            ]

instance PlutusTx.FromData SundaeAddress where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [paymentCredential, stakeCredential]) ->
            SundaeAddress
                <$> PlutusTx.fromBuiltinData paymentCredential
                <*> PlutusTx.fromBuiltinData stakeCredential
        _ -> Nothing

data SundaeDatum
    = NoDatum
    | DatumHash (C.Hash C.ScriptData)
    | InlineDatum PlutusTx.BuiltinData
    deriving stock (Eq, Show)

instance Arbitrary SundaeDatum where
    arbitrary =
        Gen.oneof
            [ pure NoDatum
            , DatumHash <$> H.hedgehog Gen.genHashScriptData
            , InlineDatum <$> Gen.elements [PlutusTx.mkConstr 0 [], PlutusTx.mkI 0]
            ]

instance PlutusTx.ToData SundaeDatum where
    toBuiltinData = \case
        NoDatum -> PlutusTx.mkConstr 0 []
        DatumHash hsh -> PlutusTx.mkConstr 1 [D.serialiseHash hsh]
        InlineDatum dt -> PlutusTx.mkConstr 2 [dt]

instance PlutusTx.FromData SundaeDatum where
    fromBuiltinData k = D.withConstr k $ \case
        (0, []) -> Just NoDatum
        (1, [D.getB >=> D.deserialiseHash -> Just hsh]) -> Just (DatumHash hsh)
        (2, [dt]) -> Just (InlineDatum dt)
        _ -> Nothing

data Destination
    = FixedDestination SundaeAddress SundaeDatum
    | SelfDestination
    deriving stock (Eq, Show)

instance Arbitrary Destination where
    arbitrary =
        Gen.oneof
            [ FixedDestination <$> arbitrary <*> arbitrary
            , pure SelfDestination
            ]

instance PlutusTx.ToData Destination where
    toBuiltinData = \case
        FixedDestination address datum ->
            PlutusTx.mkConstr
                0
                [ PlutusTx.toBuiltinData address
                , PlutusTx.toBuiltinData datum
                ]
        SelfDestination -> PlutusTx.mkConstr 1 []

instance PlutusTx.FromData Destination where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [address, datum]) ->
            FixedDestination
                <$> PlutusTx.fromBuiltinData address
                <*> PlutusTx.fromBuiltinData datum
        (1, []) -> Just SelfDestination
        _ -> Nothing

data StrategyAuthorization
    = StrategySignature (C.Hash C.PaymentKey)
    | StrategyScript PlutusTx.BuiltinByteString
    deriving stock (Eq, Show)

instance Arbitrary StrategyAuthorization where
    arbitrary =
        Gen.oneof
            [ StrategySignature <$> H.hedgehog (Gen.genVerificationKeyHash C.AsPaymentKey)
            , StrategyScript . PlutusTx.toBuiltin . C.serialiseToRawBytes <$> H.hedgehog Gen.genScriptHash
            ]

instance PlutusTx.ToData StrategyAuthorization where
    toBuiltinData = \case
        StrategySignature signer -> PlutusTx.mkConstr 0 [D.serialiseHash signer]
        StrategyScript script -> PlutusTx.mkConstr 1 [PlutusTx.mkB script]

instance PlutusTx.FromData StrategyAuthorization where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [D.getB >=> D.deserialiseHash -> Just signer]) -> Just (StrategySignature signer)
        (1, [D.getB -> Just script]) -> Just (StrategyScript script)
        _ -> Nothing

data AssetAmount = AssetAmount
    { aaAssetId :: C.AssetId
    , aaAmount :: C.Quantity
    }
    deriving stock (Eq, Show)

instance Arbitrary AssetAmount where
    arbitrary =
        AssetAmount <$> H.hedgehog Gen.genAssetId <*> H.hedgehog Gen.genPositiveQuantity

instance PlutusTx.ToData AssetAmount where
    toBuiltinData AssetAmount{aaAssetId, aaAmount = C.Quantity aaAmount} =
        let (policyId, assetName) = serialiseAsset aaAssetId
         in PlutusTx.mkList [PlutusTx.mkB policyId, PlutusTx.mkB assetName, PlutusTx.mkI aaAmount]

instance PlutusTx.FromData AssetAmount where
    fromBuiltinData dt = withList dt $ \case
        [D.getB -> Just policyId, D.getB -> Just assetName, D.getI -> Just aaAmount] ->
            AssetAmount
                <$> deserialiseAsset policyId assetName
                <*> pure (C.Quantity aaAmount)
        _ -> Nothing

data Order
    = Strategy StrategyAuthorization
    | Swap AssetAmount AssetAmount
    | Deposit AssetAmount AssetAmount
    | Withdrawal AssetAmount
    | Donation AssetAmount AssetAmount
    | Record C.AssetId
    deriving stock (Eq, Show)

instance Arbitrary Order where
    arbitrary =
        Gen.oneof
            [ Strategy <$> arbitrary
            , Swap <$> arbitrary <*> arbitrary
            , Deposit <$> arbitrary <*> arbitrary
            , Withdrawal <$> arbitrary
            , Donation <$> arbitrary <*> arbitrary
            , Record <$> H.hedgehog Gen.genAssetId
            ]

instance PlutusTx.ToData Order where
    toBuiltinData = \case
        Strategy auth -> PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData auth]
        Swap offer minReceived ->
            PlutusTx.mkConstr
                1
                [ PlutusTx.toBuiltinData offer
                , PlutusTx.toBuiltinData minReceived
                ]
        Deposit assetA assetB ->
            PlutusTx.mkConstr
                2
                [ PlutusTx.mkList [PlutusTx.toBuiltinData assetA, PlutusTx.toBuiltinData assetB]
                ]
        Withdrawal amount -> PlutusTx.mkConstr 3 [PlutusTx.toBuiltinData amount]
        Donation assetA assetB ->
            PlutusTx.mkConstr
                4
                [ PlutusTx.mkList [PlutusTx.toBuiltinData assetA, PlutusTx.toBuiltinData assetB]
                ]
        Record policy ->
            let (policyId, assetName) = serialiseAsset policy
             in PlutusTx.mkConstr 5 [PlutusTx.mkList [PlutusTx.mkB policyId, PlutusTx.mkB assetName]]

instance PlutusTx.FromData Order where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [auth]) -> Strategy <$> PlutusTx.fromBuiltinData auth
        (1, [offer, minReceived]) ->
            Swap
                <$> PlutusTx.fromBuiltinData offer
                <*> PlutusTx.fromBuiltinData minReceived
        (2, [assets]) -> withList assets $ \case
            [assetA, assetB] ->
                Deposit
                    <$> PlutusTx.fromBuiltinData assetA
                    <*> PlutusTx.fromBuiltinData assetB
            _ -> Nothing
        (3, [amount]) -> Withdrawal <$> PlutusTx.fromBuiltinData amount
        (4, [assets]) -> withList assets $ \case
            [assetA, assetB] ->
                Donation
                    <$> PlutusTx.fromBuiltinData assetA
                    <*> PlutusTx.fromBuiltinData assetB
            _ -> Nothing
        (5, [policy]) -> do
            [D.getB -> Just policyId, D.getB -> Just assetName] <- withList policy pure
            Record <$> deserialiseAsset policyId assetName
        _ -> Nothing

data SundaeOrderDatum = SundaeOrderDatum
    { soPoolIdent :: Maybe PlutusTx.BuiltinByteString
    , soOwner :: MultisigScript
    , soMaxProtocolFee :: C.Quantity
    , soDestination :: Destination
    , soDetails :: Order
    , soExtension :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Show)

instance Arbitrary SundaeOrderDatum where
    arbitrary =
        SundaeOrderDatum
            <$> Gen.oneof [pure Nothing, Just . PlutusTx.toBuiltin . C.serialiseToRawBytes <$> H.hedgehog Gen.genScriptHash]
            <*> arbitrary
            <*> H.hedgehog Gen.genPositiveQuantity
            <*> arbitrary
            <*> arbitrary
            <*> (PlutusTx.toBuiltin . C.serialiseToRawBytes <$> H.hedgehog Gen.genScriptHash)

instance PlutusTx.ToData SundaeOrderDatum where
    toBuiltinData SundaeOrderDatum{soPoolIdent, soOwner, soMaxProtocolFee = C.Quantity soMaxProtocolFee, soDestination, soDetails, soExtension} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.toBuiltinData soPoolIdent
            , PlutusTx.toBuiltinData soOwner
            , PlutusTx.mkI soMaxProtocolFee
            , PlutusTx.toBuiltinData soDestination
            , PlutusTx.toBuiltinData soDetails
            , PlutusTx.mkB soExtension
            ]

instance PlutusTx.FromData SundaeOrderDatum where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [poolIdent, owner, maxProtocolFee, destination, details, extension]) ->
            SundaeOrderDatum
                <$> PlutusTx.fromBuiltinData poolIdent
                <*> PlutusTx.fromBuiltinData owner
                <*> fmap C.Quantity (D.getI maxProtocolFee)
                <*> PlutusTx.fromBuiltinData destination
                <*> PlutusTx.fromBuiltinData details
                <*> D.getB extension
        _ -> Nothing

serialiseAsset :: C.AssetId -> (PlutusTx.BuiltinByteString, PlutusTx.BuiltinByteString)
serialiseAsset = \case
    C.AdaAssetId -> (PlutusTx.toBuiltin BS.empty, PlutusTx.toBuiltin BS.empty)
    C.AssetId policyId assetName ->
        ( PlutusTx.toBuiltin (C.serialiseToRawBytes policyId)
        , PlutusTx.toBuiltin (C.serialiseToRawBytes assetName)
        )

deserialiseAsset :: PlutusTx.BuiltinByteString -> PlutusTx.BuiltinByteString -> Maybe C.AssetId
deserialiseAsset policyId assetName
    | policyId == PlutusTx.toBuiltin BS.empty && assetName == PlutusTx.toBuiltin BS.empty = Just C.AdaAssetId
    | otherwise =
        C.AssetId
            <$> either (const Nothing) Just (C.deserialiseFromRawBytes C.AsPolicyId $ PlutusTx.fromBuiltin policyId)
            <*> either (const Nothing) Just (C.deserialiseFromRawBytes C.AsAssetName $ PlutusTx.fromBuiltin assetName)

withList :: PlutusTx.BuiltinData -> ([PlutusTx.BuiltinData] -> Maybe a) -> Maybe a
withList dt match =
    PlutusTx.matchData
        dt
        (\_ _ -> Nothing)
        (const Nothing)
        match
        (const Nothing)
        (const Nothing)
