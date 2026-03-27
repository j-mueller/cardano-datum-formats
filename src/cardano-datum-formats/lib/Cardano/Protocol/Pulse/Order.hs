{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Pulse.Order (
    OrderDatum (..),
) where

import Cardano.Address.Aiken (AikenAddress)
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (stripFieldPrefix, sumOptionsWithFieldModifier)
import Cardano.Protocol.JSON ()
import Cardano.Protocol.Pulse.Common (
    PubKeyHash,
    maybeFromOptionData,
    maybeFromOptionRawData,
    maybeToOptionData,
    maybeToOptionRawData,
 )
import Cardano.Transaction.OutputReference (OutputReference)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data OrderDatum
    = OSplitSY
        { oOwnerPkh :: PubKeyHash
        , oReceivePtAddr :: AikenAddress
        , oReceivePtDatum :: Maybe PlutusTx.BuiltinData
        , oReceiveYtAddr :: AikenAddress
        , oReceiveYtDatum :: Maybe PlutusTx.BuiltinData
        }
    | OMergeSY
        { oOwnerPkh :: PubKeyHash
        , oReceiveAddr :: AikenAddress
        , oReceiveDatum :: Maybe PlutusTx.BuiltinData
        }
    | OInitLP
        { oOwnerPkh :: PubKeyHash
        , oOwnerStakeKey :: Maybe PubKeyHash
        }
    | OMintLP
        { oOwnerPkh :: PubKeyHash
        , oReceiveAddr :: AikenAddress
        , oReceiveDatum :: Maybe PlutusTx.BuiltinData
        }
    | OBurnLP
        { oOwnerPkh :: PubKeyHash
        , oReceiveSyAddr :: AikenAddress
        , oReceiveSyDatum :: Maybe PlutusTx.BuiltinData
        , oReceivePtAddr :: AikenAddress
        , oReceivePtDatum :: Maybe PlutusTx.BuiltinData
        }
    | OSwapExactSYForPT
        { oOwnerPkh :: PubKeyHash
        , oReceiveAddr :: AikenAddress
        , oReceiveDatum :: Maybe PlutusTx.BuiltinData
        , oMinPtOut :: Integer
        }
    | OSwapExactPTForSY
        { oOwnerPkh :: PubKeyHash
        , oReceiveAddr :: AikenAddress
        , oReceiveDatum :: Maybe PlutusTx.BuiltinData
        , oMinSyOut :: Integer
        }
    | OSwapExactYTForSY
        { oOwnerPkh :: PubKeyHash
        , oReceiveAddr :: AikenAddress
        , oReceiveDatum :: Maybe PlutusTx.BuiltinData
        , oMinSyOut :: Integer
        }
    | OSwapExactSYForYT
        { oOwnerPkh :: PubKeyHash
        , oReceiveAddr :: AikenAddress
        , oReceiveDatum :: Maybe PlutusTx.BuiltinData
        , oMinYtOut :: Integer
        }
    | OWithdrawYTReward
        { oOwnerPkh :: PubKeyHash
        , oStakeId :: OutputReference
        }
    | OStakeYT
        { oOwnerPkh :: PubKeyHash
        , oMaybeStakeId :: Maybe OutputReference
        }
    deriving stock (Eq, Show, Generic)

instance Arbitrary OrderDatum where
    arbitrary =
        Gen.oneof
            [ OSplitSY <$> arbitrary <*> arbitrary <*> arbitraryMaybeData <*> arbitrary <*> arbitraryMaybeData
            , OMergeSY <$> arbitrary <*> arbitrary <*> arbitraryMaybeData
            , OInitLP <$> arbitrary <*> arbitrary
            , OMintLP <$> arbitrary <*> arbitrary <*> arbitraryMaybeData
            , OBurnLP <$> arbitrary <*> arbitrary <*> arbitraryMaybeData <*> arbitrary <*> arbitraryMaybeData
            , OSwapExactSYForPT <$> arbitrary <*> arbitrary <*> arbitraryMaybeData <*> arbitrary
            , OSwapExactPTForSY <$> arbitrary <*> arbitrary <*> arbitraryMaybeData <*> arbitrary
            , OSwapExactYTForSY <$> arbitrary <*> arbitrary <*> arbitraryMaybeData <*> arbitrary
            , OSwapExactSYForYT <$> arbitrary <*> arbitrary <*> arbitraryMaybeData <*> arbitrary
            , OWithdrawYTReward <$> arbitrary <*> arbitrary
            , OStakeYT <$> arbitrary <*> arbitrary
            ]

instance PlutusTx.ToData OrderDatum where
    toBuiltinData = \case
        OSplitSY oOwnerPkh oReceivePtAddr oReceivePtDatum oReceiveYtAddr oReceiveYtDatum ->
            PlutusTx.mkConstr
                0
                [ PlutusTx.toBuiltinData oOwnerPkh
                , PlutusTx.toBuiltinData oReceivePtAddr
                , maybeToOptionRawData oReceivePtDatum
                , PlutusTx.toBuiltinData oReceiveYtAddr
                , maybeToOptionRawData oReceiveYtDatum
                ]
        OMergeSY oOwnerPkh oReceiveAddr oReceiveDatum ->
            PlutusTx.mkConstr
                1
                [ PlutusTx.toBuiltinData oOwnerPkh
                , PlutusTx.toBuiltinData oReceiveAddr
                , maybeToOptionRawData oReceiveDatum
                ]
        OInitLP oOwnerPkh oOwnerStakeKey ->
            PlutusTx.mkConstr
                2
                [ PlutusTx.toBuiltinData oOwnerPkh
                , maybeToOptionData oOwnerStakeKey
                ]
        OMintLP oOwnerPkh oReceiveAddr oReceiveDatum ->
            PlutusTx.mkConstr
                3
                [ PlutusTx.toBuiltinData oOwnerPkh
                , PlutusTx.toBuiltinData oReceiveAddr
                , maybeToOptionRawData oReceiveDatum
                ]
        OBurnLP oOwnerPkh oReceiveSyAddr oReceiveSyDatum oReceivePtAddr oReceivePtDatum ->
            PlutusTx.mkConstr
                4
                [ PlutusTx.toBuiltinData oOwnerPkh
                , PlutusTx.toBuiltinData oReceiveSyAddr
                , maybeToOptionRawData oReceiveSyDatum
                , PlutusTx.toBuiltinData oReceivePtAddr
                , maybeToOptionRawData oReceivePtDatum
                ]
        OSwapExactSYForPT oOwnerPkh oReceiveAddr oReceiveDatum oMinPtOut ->
            PlutusTx.mkConstr
                5
                [ PlutusTx.toBuiltinData oOwnerPkh
                , PlutusTx.toBuiltinData oReceiveAddr
                , maybeToOptionRawData oReceiveDatum
                , PlutusTx.mkI oMinPtOut
                ]
        OSwapExactPTForSY oOwnerPkh oReceiveAddr oReceiveDatum oMinSyOut ->
            PlutusTx.mkConstr
                6
                [ PlutusTx.toBuiltinData oOwnerPkh
                , PlutusTx.toBuiltinData oReceiveAddr
                , maybeToOptionRawData oReceiveDatum
                , PlutusTx.mkI oMinSyOut
                ]
        OSwapExactYTForSY oOwnerPkh oReceiveAddr oReceiveDatum oMinSyOut ->
            PlutusTx.mkConstr
                7
                [ PlutusTx.toBuiltinData oOwnerPkh
                , PlutusTx.toBuiltinData oReceiveAddr
                , maybeToOptionRawData oReceiveDatum
                , PlutusTx.mkI oMinSyOut
                ]
        OSwapExactSYForYT oOwnerPkh oReceiveAddr oReceiveDatum oMinYtOut ->
            PlutusTx.mkConstr
                8
                [ PlutusTx.toBuiltinData oOwnerPkh
                , PlutusTx.toBuiltinData oReceiveAddr
                , maybeToOptionRawData oReceiveDatum
                , PlutusTx.mkI oMinYtOut
                ]
        OWithdrawYTReward oOwnerPkh oStakeId ->
            PlutusTx.mkConstr
                9
                [ PlutusTx.toBuiltinData oOwnerPkh
                , PlutusTx.toBuiltinData oStakeId
                ]
        OStakeYT oOwnerPkh oMaybeStakeId ->
            PlutusTx.mkConstr
                10
                [ PlutusTx.toBuiltinData oOwnerPkh
                , maybeToOptionData oMaybeStakeId
                ]

instance PlutusTx.FromData OrderDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [ownerPkh, receivePtAddr, receivePtDatum, receiveYtAddr, receiveYtDatum]) ->
            OSplitSY
                <$> PlutusTx.fromBuiltinData ownerPkh
                <*> PlutusTx.fromBuiltinData receivePtAddr
                <*> maybeFromOptionRawData receivePtDatum
                <*> PlutusTx.fromBuiltinData receiveYtAddr
                <*> maybeFromOptionRawData receiveYtDatum
        (1, [ownerPkh, receiveAddr, receiveDatum]) ->
            OMergeSY
                <$> PlutusTx.fromBuiltinData ownerPkh
                <*> PlutusTx.fromBuiltinData receiveAddr
                <*> maybeFromOptionRawData receiveDatum
        (2, [ownerPkh, ownerStakeKey]) ->
            OInitLP
                <$> PlutusTx.fromBuiltinData ownerPkh
                <*> maybeFromOptionData PlutusTx.fromBuiltinData ownerStakeKey
        (3, [ownerPkh, receiveAddr, receiveDatum]) ->
            OMintLP
                <$> PlutusTx.fromBuiltinData ownerPkh
                <*> PlutusTx.fromBuiltinData receiveAddr
                <*> maybeFromOptionRawData receiveDatum
        (4, [ownerPkh, receiveSyAddr, receiveSyDatum, receivePtAddr, receivePtDatum]) ->
            OBurnLP
                <$> PlutusTx.fromBuiltinData ownerPkh
                <*> PlutusTx.fromBuiltinData receiveSyAddr
                <*> maybeFromOptionRawData receiveSyDatum
                <*> PlutusTx.fromBuiltinData receivePtAddr
                <*> maybeFromOptionRawData receivePtDatum
        (5, [ownerPkh, receiveAddr, receiveDatum, D.getI -> Just oMinPtOut]) ->
            OSwapExactSYForPT
                <$> PlutusTx.fromBuiltinData ownerPkh
                <*> PlutusTx.fromBuiltinData receiveAddr
                <*> maybeFromOptionRawData receiveDatum
                <*> pure oMinPtOut
        (6, [ownerPkh, receiveAddr, receiveDatum, D.getI -> Just oMinSyOut]) ->
            OSwapExactPTForSY
                <$> PlutusTx.fromBuiltinData ownerPkh
                <*> PlutusTx.fromBuiltinData receiveAddr
                <*> maybeFromOptionRawData receiveDatum
                <*> pure oMinSyOut
        (7, [ownerPkh, receiveAddr, receiveDatum, D.getI -> Just oMinSyOut]) ->
            OSwapExactYTForSY
                <$> PlutusTx.fromBuiltinData ownerPkh
                <*> PlutusTx.fromBuiltinData receiveAddr
                <*> maybeFromOptionRawData receiveDatum
                <*> pure oMinSyOut
        (8, [ownerPkh, receiveAddr, receiveDatum, D.getI -> Just oMinYtOut]) ->
            OSwapExactSYForYT
                <$> PlutusTx.fromBuiltinData ownerPkh
                <*> PlutusTx.fromBuiltinData receiveAddr
                <*> maybeFromOptionRawData receiveDatum
                <*> pure oMinYtOut
        (9, [ownerPkh, stakeId]) ->
            OWithdrawYTReward
                <$> PlutusTx.fromBuiltinData ownerPkh
                <*> PlutusTx.fromBuiltinData stakeId
        (10, [ownerPkh, stakeId]) ->
            OStakeYT
                <$> PlutusTx.fromBuiltinData ownerPkh
                <*> maybeFromOptionData PlutusTx.fromBuiltinData stakeId
        _ -> Nothing

arbitraryMaybeData :: QC.Gen (Maybe PlutusTx.BuiltinData)
arbitraryMaybeData =
    Gen.oneof
        [ pure Nothing
        , Just <$> Gen.elements [PlutusTx.mkConstr 0 [], PlutusTx.mkI 0]
        ]

pulseOrderDatumOptions :: Aeson.Options
pulseOrderDatumOptions =
    sumOptionsWithFieldModifier 0 (stripFieldPrefix "o")

instance ToJSON OrderDatum where
    toJSON = Aeson.genericToJSON pulseOrderDatumOptions
    toEncoding = Aeson.genericToEncoding pulseOrderDatumOptions

instance FromJSON OrderDatum where
    parseJSON = Aeson.genericParseJSON pulseOrderDatumOptions

$(deriveTypeScript (sumOptionsWithFieldModifier 0 (stripFieldPrefix "o")) ''OrderDatum)

instance Schema.ToSchema OrderDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions pulseOrderDatumOptions)
