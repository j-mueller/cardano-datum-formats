module Cardano.Protocol.MuesliSwap.Test (
    tests,
) where

import Cardano.Asset qualified as Asset
import Cardano.Data qualified as Datum
import Cardano.Protocol.MuesliSwap.Order qualified as Order
import Cardano.Protocol.MuesliSwap.Pool qualified as Pool
import Cardano.Test.Utils (datumRoundTrip, hexToBuiltinByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "muesli-swap"
        [ testGroup
            "datum roundtrips"
            [ testProperty "PoolDatum" (datumRoundTrip @Pool.PoolDatum)
            , testProperty "OrderStep" (datumRoundTrip @Order.OrderStep)
            , testProperty "OrderDatum" (datumRoundTrip @Order.OrderDatum)
            ]
        , testGroup
            "mainnet decode"
            [ HUnit.testCase "decode live test pool datum" decodeTestPoolDatum
            , HUnit.testCase "decode live PRSPR pool datum" decodePrsprPoolDatum
            , HUnit.testCase "decode live deposit order datum" decodeDepositOrderDatum
            , HUnit.testCase "decode live withdraw order datum" decodeWithdrawOrderDatum
            ]
        ]

decodeTestPoolDatum :: HUnit.Assertion
decodeTestPoolDatum = do
    let poolHex = "d8799fd8799f4040ffd8799f581ca8512101cb1163cc218e616bb4d4070349a1c9395313f1323cc58363524d7565736c695377617054657374506f6f6cff1a001c8bed181eff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Pool.PoolDatum poolHex)
    HUnit.assertEqual "coin A policy" (hexToBuiltinByteString "") (Asset.aPolicyId $ Pool.pdCoinA datum)
    HUnit.assertEqual "coin A name" (hexToBuiltinByteString "") (Asset.aTokenName $ Pool.pdCoinA datum)
    HUnit.assertEqual "coin B policy" (hexToBuiltinByteString "a8512101cb1163cc218e616bb4d4070349a1c9395313f1323cc58363") (Asset.aPolicyId $ Pool.pdCoinB datum)
    HUnit.assertEqual "coin B name" (hexToBuiltinByteString "4d7565736c695377617054657374506f6f6c") (Asset.aTokenName $ Pool.pdCoinB datum)
    HUnit.assertEqual "total liquidity" 1870829 (Pool.pdTotalLiquidity datum)
    HUnit.assertEqual "swap fee" 30 (Pool.pdSwapFee datum)

decodePrsprPoolDatum :: HUnit.Assertion
decodePrsprPoolDatum = do
    let poolHex = "d8799fd8799f4040ffd8799f581c52489ea87bbceaf6375cc22f74c19382a3d5da3f8b9b15d2537044b9455052535052ff1a00016a2b181eff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Pool.PoolDatum poolHex)
    HUnit.assertEqual "coin A policy" (hexToBuiltinByteString "") (Asset.aPolicyId $ Pool.pdCoinA datum)
    HUnit.assertEqual "coin A name" (hexToBuiltinByteString "") (Asset.aTokenName $ Pool.pdCoinA datum)
    HUnit.assertEqual "coin B policy" (hexToBuiltinByteString "52489ea87bbceaf6375cc22f74c19382a3d5da3f8b9b15d2537044b9") (Asset.aPolicyId $ Pool.pdCoinB datum)
    HUnit.assertEqual "coin B name" (hexToBuiltinByteString "5052535052") (Asset.aTokenName $ Pool.pdCoinB datum)
    HUnit.assertEqual "total liquidity" 92715 (Pool.pdTotalLiquidity datum)
    HUnit.assertEqual "swap fee" 30 (Pool.pdSwapFee datum)

decodeDepositOrderDatum :: HUnit.Assertion
decodeDepositOrderDatum = do
    let orderHex = "d8799fd8799fd8799f581c353b8bc29a15603f0b73eac44653d1bd944d92e0e0dcd5eb185164a2ffd8799fd8799fd8799f581cda22c532206a75a628778eebaf63826f9d93fbe9b4ac69a7f8e4cd78ffffffffd8799fd8799f581c353b8bc29a15603f0b73eac44653d1bd944d92e0e0dcd5eb185164a2ffd8799fd8799fd8799f581cda22c532206a75a628778eebaf63826f9d93fbe9b4ac69a7f8e4cd78ffffffffd87a80d8799f1a00069603ff1a001e84801a001e84804e4d7565736c69537761705f414d4dff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Order.OrderDatum orderHex)
    case Order.odStep datum of
        Order.Deposit{dMinimumLP} ->
            HUnit.assertEqual "minimum LP" 431619 dMinimumLP
        step ->
            HUnit.assertFailure ("expected Deposit, got " <> show step)
    HUnit.assertEqual "receiver datum hash" Nothing (Order.odReceiverDatumHash datum)
    HUnit.assertEqual "batcher fee" 2000000 (Order.odBatcherFee datum)
    HUnit.assertEqual "output ADA" 2000000 (Order.odOutputADA datum)
    HUnit.assertEqual "pool NFT token name" (hexToBuiltinByteString "") (Order.odPoolNftTokenName datum)
    HUnit.assertEqual "script version" Order.scriptVersion (Order.odScriptVersion datum)

decodeWithdrawOrderDatum :: HUnit.Assertion
decodeWithdrawOrderDatum = do
    let orderHex = "d8799fd8799fd8799f581c21babefc88e9c541cef25c0c4afd553768a4a4a431e93f41f719d322ffd8799fd8799fd8799f581c192d58ab285961cda93a72b9544405497ff7bd87ce75e46bdffb7571ffffffffd8799fd8799f581c21babefc88e9c541cef25c0c4afd553768a4a4a431e93f41f719d322ffd8799fd8799fd8799f581c192d58ab285961cda93a72b9544405497ff7bd87ce75e46bdffb7571ffffffffd87a80d87a9f0015ff1a001e84801a001e84805820063203a165a7df292ee114897aa04d23072ab46e1e5b4b683bd663d1327210e74e4d7565736c69537761705f414d4dff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Order.OrderDatum orderHex)
    case Order.odStep datum of
        Order.Withdraw{wMinimumCoinA, wMinimumCoinB} -> do
            HUnit.assertEqual "minimum coin A" 0 wMinimumCoinA
            HUnit.assertEqual "minimum coin B" 21 wMinimumCoinB
        step ->
            HUnit.assertFailure ("expected Withdraw, got " <> show step)
    HUnit.assertEqual "receiver datum hash" Nothing (Order.odReceiverDatumHash datum)
    HUnit.assertEqual "batcher fee" 2000000 (Order.odBatcherFee datum)
    HUnit.assertEqual "output ADA" 2000000 (Order.odOutputADA datum)
    HUnit.assertEqual "pool NFT token name" (hexToBuiltinByteString "063203a165a7df292ee114897aa04d23072ab46e1e5b4b683bd663d1327210e7") (Order.odPoolNftTokenName datum)
    HUnit.assertEqual "script version" Order.scriptVersion (Order.odScriptVersion datum)
