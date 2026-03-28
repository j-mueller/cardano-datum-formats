{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.MuesliSwap.Order (
    OrderStep (..),
    OrderDatum (..),
    scriptVersion,
) where

import Cardano.Address.Plutus (PlutusAddress)
import Cardano.Api qualified as C
import Cardano.Asset (Asset)
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions, stripFieldPrefixes, sumOptionsWithFieldModifier)
import Cardano.Protocol.JSON ()
import Control.Monad ((>=>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.ByteString qualified as BS
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
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
    deriving stock (Eq, Show, Generic)

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
    deriving stock (Eq, Show, Generic)

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

muesliOrderStepOptions :: Aeson.Options
muesliOrderStepOptions =
    sumOptionsWithFieldModifier 0 (stripFieldPrefixes ["d", "w", "osd"])

instance ToJSON OrderStep where
    toJSON = Aeson.genericToJSON muesliOrderStepOptions
    toEncoding = Aeson.genericToEncoding muesliOrderStepOptions

instance FromJSON OrderStep where
    parseJSON = Aeson.genericParseJSON muesliOrderStepOptions

$(deriveTypeScript (sumOptionsWithFieldModifier 0 (stripFieldPrefixes ["d", "w", "osd"])) ''OrderStep)

instance Schema.ToSchema OrderStep where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions muesliOrderStepOptions)

instance ToJSON OrderDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON OrderDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''OrderDatum)

instance Schema.ToSchema OrderDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
