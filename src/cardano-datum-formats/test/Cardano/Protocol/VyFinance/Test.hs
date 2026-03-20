module Cardano.Protocol.VyFinance.Test (
    tests,
) where

import Cardano.Data qualified as Datum
import Cardano.Protocol.VyFinance.Pool qualified as Pool
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

import Cardano.Test.Utils (datumRoundTrip)

tests :: TestTree
tests =
    testGroup
        "vyfinance"
        [ testGroup
            "datum roundtrips"
            [ testProperty "PoolDatum" (datumRoundTrip @Pool.PoolDatum)
            ]
        , testGroup
            "mainnet decode"
            [ HUnit.testCase "decode live ADA/VYFI pool datum" decodeAdaVyfiPoolDatum
            , HUnit.testCase "decode live xVYFI/TOKE pool datum" decodeXVyfiTokePoolDatum
            , HUnit.testCase "decode live DJED/Book of Sugar pool datum" decodeDjedBookOfSugarPoolDatum
            ]
        ]

decodeAdaVyfiPoolDatum :: HUnit.Assertion
decodeAdaVyfiPoolDatum = do
    let poolHex = "d8799f1a286956af1a0a47976a1b000001885b9b19f8ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Pool.PoolDatum poolHex)
    HUnit.assertEqual "asset A quantity" 677992111 (Pool.pdAssetAQuantity datum)
    HUnit.assertEqual "asset B quantity" 172463978 (Pool.pdAssetBQuantity datum)
    HUnit.assertEqual "lp quantity" 1685164071416 (Pool.pdLpQuantity datum)

decodeXVyfiTokePoolDatum :: HUnit.Assertion
decodeXVyfiTokePoolDatum = do
    let poolHex = "d8799f1a00223f690c1a000e8494ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Pool.PoolDatum poolHex)
    HUnit.assertEqual "asset A quantity" 2244457 (Pool.pdAssetAQuantity datum)
    HUnit.assertEqual "asset B quantity" 12 (Pool.pdAssetBQuantity datum)
    HUnit.assertEqual "lp quantity" 951444 (Pool.pdLpQuantity datum)

decodeDjedBookOfSugarPoolDatum :: HUnit.Assertion
decodeDjedBookOfSugarPoolDatum = do
    let poolHex = "d8799f001917e20aff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Pool.PoolDatum poolHex)
    HUnit.assertEqual "asset A quantity" 0 (Pool.pdAssetAQuantity datum)
    HUnit.assertEqual "asset B quantity" 6114 (Pool.pdAssetBQuantity datum)
    HUnit.assertEqual "lp quantity" 10 (Pool.pdLpQuantity datum)
