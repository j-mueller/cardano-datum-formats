module Cardano.Protocol.MuesliSwap.Order (
    OrderStep (..),
    OrderDatum (..),
    scriptVersion,
) where

import Cardano.Address.Plutus (PlutusAddress)
import Cardano.Api qualified as C
import Cardano.Asset (Asset)
import Cardano.Data qualified as D
import Control.Monad ((>=>))
import Data.ByteString qualified as BS
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Hedgehog qualified as H

data OrderStep
    = Deposit
        { dMinimumLP :: Integer
        }
    | Withdraw
        { wMinimumCoinA :: Integer
        , wMinimumCoinB :: Integer
        }
    | OneSideDeposit
        { osdDesiredCoin :: Asset
        , osdMinimumLP :: Integer
        }
    deriving stock (Eq, Show)

data OrderDatum = OrderDatum
    { odSender :: PlutusAddress
    , odReceiver :: PlutusAddress
    , odReceiverDatumHash :: Maybe (C.Hash C.ScriptData)
    , odStep :: OrderStep
    , odBatcherFee :: Integer
    , odOutputADA :: Integer
    , odPoolNftTokenName :: PlutusTx.BuiltinByteString
    , odScriptVersion :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Show)

scriptVersion :: PlutusTx.BuiltinByteString
scriptVersion = "MuesliSwap_AMM"

instance Arbitrary OrderStep where
    arbitrary =
        Gen.oneof
            [ Deposit <$> arbitrary
            , Withdraw <$> arbitrary <*> arbitrary
            , OneSideDeposit <$> arbitrary <*> arbitrary
            ]

instance Arbitrary OrderDatum where
    arbitrary =
        OrderDatum
            <$> arbitrary
            <*> arbitrary
            <*> (Gen.oneof [pure Nothing, Just <$> H.hedgehog Gen.genHashScriptData])
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryAnyBuiltinByteString
            <*> arbitraryAnyBuiltinByteString

instance PTx.ToData OrderStep where
    toBuiltinData = \case
        Deposit dMinimumLP ->
            PlutusTx.mkConstr 0 [PlutusTx.mkI dMinimumLP]
        Withdraw wMinimumCoinA wMinimumCoinB ->
            PlutusTx.mkConstr 1 [PlutusTx.mkI wMinimumCoinA, PlutusTx.mkI wMinimumCoinB]
        OneSideDeposit osdDesiredCoin osdMinimumLP ->
            PlutusTx.mkConstr 2 [PTx.toBuiltinData osdDesiredCoin, PlutusTx.mkI osdMinimumLP]

instance PTx.FromData OrderStep where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just dMinimumLP]) ->
            pure Deposit{dMinimumLP}
        (1, [D.getI -> Just wMinimumCoinA, D.getI -> Just wMinimumCoinB]) ->
            pure Withdraw{wMinimumCoinA, wMinimumCoinB}
        (2, [desiredCoin, D.getI -> Just osdMinimumLP]) ->
            OneSideDeposit
                <$> PTx.fromBuiltinData desiredCoin
                <*> pure osdMinimumLP
        _ -> Nothing

instance PTx.ToData OrderDatum where
    toBuiltinData OrderDatum{odSender, odReceiver, odReceiverDatumHash, odStep, odBatcherFee, odOutputADA, odPoolNftTokenName, odScriptVersion} =
        PlutusTx.mkConstr
            0
            [ PTx.toBuiltinData odSender
            , PTx.toBuiltinData odReceiver
            , maybeToOptionRawData (D.serialiseHash <$> odReceiverDatumHash)
            , PTx.toBuiltinData odStep
            , PlutusTx.mkI odBatcherFee
            , PlutusTx.mkI odOutputADA
            , PlutusTx.mkB odPoolNftTokenName
            , PlutusTx.mkB odScriptVersion
            ]

instance PTx.FromData OrderDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [sender, receiver, receiverDatumHash, step, D.getI -> Just odBatcherFee, D.getI -> Just odOutputADA, D.getB -> Just odPoolNftTokenName, D.getB -> Just odScriptVersion]) ->
            OrderDatum
                <$> PTx.fromBuiltinData sender
                <*> PTx.fromBuiltinData receiver
                <*> maybeFromOptionData (D.getB >=> D.deserialiseHash) receiverDatumHash
                <*> PTx.fromBuiltinData step
                <*> pure odBatcherFee
                <*> pure odOutputADA
                <*> pure odPoolNftTokenName
                <*> pure odScriptVersion
        -- Some live Muesli deposit orders still use the older 7-field layout
        -- without an explicit pool NFT token name.
        (0, [sender, receiver, receiverDatumHash, step, D.getI -> Just odBatcherFee, D.getI -> Just odOutputADA, D.getB -> Just odScriptVersion]) ->
            OrderDatum
                <$> PTx.fromBuiltinData sender
                <*> PTx.fromBuiltinData receiver
                <*> maybeFromOptionData (D.getB >=> D.deserialiseHash) receiverDatumHash
                <*> PTx.fromBuiltinData step
                <*> pure odBatcherFee
                <*> pure odOutputADA
                <*> pure (PlutusTx.toBuiltin BS.empty)
                <*> pure odScriptVersion
        _ -> Nothing

maybeFromOptionData :: (PlutusTx.BuiltinData -> Maybe a) -> PlutusTx.BuiltinData -> Maybe (Maybe a)
maybeFromOptionData decode dt = D.withConstr dt $ \case
    (0, [value]) -> Just <$> decode value
    (1, []) -> Just Nothing
    _ -> Nothing

maybeToOptionRawData :: Maybe PlutusTx.BuiltinData -> PlutusTx.BuiltinData
maybeToOptionRawData = \case
    Nothing -> PlutusTx.mkConstr 1 []
    Just value -> PlutusTx.mkConstr 0 [value]

arbitraryAnyBuiltinByteString :: QC.Gen PlutusTx.BuiltinByteString
arbitraryAnyBuiltinByteString = do
    size <- Gen.chooseInt (0, 64)
    PlutusTx.toBuiltin . BS.pack <$> Gen.vectorOf size arbitrary
