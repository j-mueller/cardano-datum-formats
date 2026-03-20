{-# LANGUAGE OverloadedStrings #-}

module Cardano.Protocol.MinSwap.Test (
    tests,
) where

import Cardano.Address.Plutus qualified as Address
import Cardano.Api qualified as C
import Cardano.Asset qualified as Asset
import Cardano.Data qualified as Datum
import Cardano.Protocol.MinSwap.Order qualified as Order
import Cardano.Protocol.MinSwap.Pool qualified as Pool
import Cardano.Test.Utils (datumRoundTrip)
import Data.Maybe (isJust)
import Data.String (fromString)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "minswap"
        [ testGroup
            "order datum"
            [ testProperty "AuthorizationMethod" (datumRoundTrip @Order.AuthorizationMethod)
            , testProperty "ExtraDatum" (datumRoundTrip @Order.ExtraDatum)
            , testProperty "Direction" (datumRoundTrip @Order.Direction)
            , testProperty "Killable" (datumRoundTrip @Order.Killable)
            , testProperty "DepositAmount" (datumRoundTrip @Order.DepositAmount)
            , testProperty "SwapAmount" (datumRoundTrip @Order.SwapAmount)
            , testProperty "WithdrawAmount" (datumRoundTrip @Order.WithdrawAmount)
            , testProperty "Asset" (datumRoundTrip @Asset.Asset)
            , testProperty "Route" (datumRoundTrip @Order.Route)
            , testProperty "Step" (datumRoundTrip @Order.Step)
            , testProperty "PlutusAddress" (datumRoundTrip @Address.PlutusAddress)
            , testProperty "Order" (datumRoundTrip @(Order.MinSwapV2Order Address.PlutusAddress))
            ]
        , testGroup
            "pool datum"
            [ testProperty "PoolFeeSharing" (datumRoundTrip @(Pool.PoolFeeSharing Address.PlutusAddress))
            , testProperty "PoolDatumV1" (datumRoundTrip @(Pool.PoolDatumV1 Address.PlutusAddress))
            , testProperty "BaseFee" (datumRoundTrip @Pool.BaseFee)
            , testProperty "StakeCredential" (datumRoundTrip @Address.PlutusStakeCredential)
            , testProperty "PoolDatumV2" (datumRoundTrip @Pool.PoolDatumV2)
            , HUnit.testCase "decode stake credential" decodeStakeCredential
            , HUnit.testCase "decode pool datum" decodePoolState
            , HUnit.testCase "decode valid order datum" decodeValidOrder
            ]
        ]

mkB' :: String -> PlutusTx.BuiltinData
mkB' = PlutusTx.mkB . fromString

testDatum :: PlutusTx.BuiltinData
testDatum =
    PlutusTx.mkConstr
        0
        [ PlutusTx.mkConstr 0 [PlutusTx.mkConstr 1 [mkB' "\251\&9\234k\185u\234m\228\162\197\NAKr#M\197\132\200\155\236\204\t\164\153\&48\158Q"]]
        , PlutusTx.mkConstr 0 [mkB' "\STXT\166\255\167\142\219\ETX\234\137\&3\219\212\202\a\135X\219\252\SI\198\187\r(\183\169\200\159", mkB' "DJED"]
        , PlutusTx.mkConstr 0 [mkB' "\225l-\200\174\147~\141\&7\144\199\253qh\215\185\148b\ESC\161L\161\DC4\NAK\243\159\237r", mkB' "MIN"]
        , PlutusTx.mkI 10000000
        , PlutusTx.mkI 34715
        , PlutusTx.mkI 4589025683
        , PlutusTx.mkI 30
        , PlutusTx.mkI 30
        , PlutusTx.mkConstr 0 [PlutusTx.mkI 1666]
        , PlutusTx.mkConstr 0 []
        ]

decodePoolState :: HUnit.Assertion
decodePoolState =
    let result = PlutusTx.fromBuiltinData @Pool.PoolDatumV2 testDatum
     in HUnit.assertBool "should decode PoolDatumV2" (isJust result)

decodeStakeCredential :: HUnit.Assertion
decodeStakeCredential =
    let dat = PlutusTx.mkConstr 0 [PlutusTx.mkConstr 1 [mkB' "\251\&9\234k\185u\234m\228\162\197\NAKr#M\197\132\200\155\236\204\t\164\153\&48\158Q"]]
        result = PlutusTx.fromBuiltinData @Address.PlutusStakeCredential dat
     in HUnit.assertBool "should decode PlutusStakeCredential" (isJust result)

decodeValidOrder :: HUnit.Assertion
decodeValidOrder = do
    let testHex =
            "d8799fd8799f581c583f6910fa82e5ee3d3f065b7f41e79f178e58cac79e2f9099de195dffd8799fd8799f581c583f6910fa82e5ee3d3f065b7f41e79f178e58cac79e2f9099de195dffd8799fd8799fd8799f581cd71ee08c96cc89e091b6a59d19d05a221f7a51fa9fe8be8b72faefdaffffffffd87980d8799fd8799f581c583f6910fa82e5ee3d3f065b7f41e79f178e58cac79e2f9099de195dffd8799fd8799fd8799f581cd71ee08c96cc89e091b6a59d19d05a221f7a51fa9fe8be8b72faefdaffffffffd87980d8799f581cd6aae2059baee188f74917493cf7637e679cd219bdfbbf4dcbeb1d0b5820f927373d3c1ac7ff660c9539da1ece0095dd2a9788fc318811e66e3a88d238b3ffd87c9fd87a80d8799f1a001eb665ff0ad87980ff1a000aae60d87a80ff"
    datum <- either HUnit.assertFailure pure (Datum.fromCborHex @C.ScriptData testHex)
    let decoded =
            PlutusTx.fromBuiltinData @(Order.MinSwapV2Order Address.PlutusAddress) $
                PlutusTx.toBuiltinData $
                    PlutusTx.dataToBuiltinData $
                        C.toPlutusData datum
    HUnit.assertBool "should decode MinSwap order datum" (isJust decoded)
