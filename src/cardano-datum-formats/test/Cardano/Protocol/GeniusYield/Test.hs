module Cardano.Protocol.GeniusYield.Test (
    tests,
) where

import Cardano.Data qualified as Datum
import Cardano.Protocol.GeniusYield.Common qualified as Common
import Cardano.Protocol.GeniusYield.PartialOrder qualified as PartialOrder
import Cardano.Test.Utils (datumRoundTrip, hexToBuiltinByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "genius-yield"
        [ testGroup
            "common"
            [ testProperty "OutputReference" (datumRoundTrip @Common.OutputReference)
            , testProperty "Credential" (datumRoundTrip @Common.Credential)
            , testProperty "StakeCredential" (datumRoundTrip @Common.StakeCredential)
            , testProperty "Address" (datumRoundTrip @Common.Address)
            , testProperty "RationalD" (datumRoundTrip @Common.RationalD)
            , testProperty "AssetClass" (datumRoundTrip @Common.AssetClass)
            , testProperty "Value" (datumRoundTrip @Common.Value)
            ]
        , testGroup
            "datum roundtrips"
            [ testProperty "PartialOrderConfigDatum" (datumRoundTrip @PartialOrder.PartialOrderConfigDatum)
            , testProperty "PartialOrderContainedFee" (datumRoundTrip @PartialOrder.PartialOrderContainedFee)
            , testProperty "PartialOrderFeeOutput" (datumRoundTrip @PartialOrder.PartialOrderFeeOutput)
            , testProperty "PartialOrderDatum" (datumRoundTrip @PartialOrder.PartialOrderDatum)
            ]
        , testGroup
            "mainnet decode"
            [ HUnit.testCase "decode live partial order config datum" decodeConfigDatum
            , HUnit.testCase "decode live partial order datum" decodeOrderDatum
            ]
        ]

decodeConfigDatum :: HUnit.Assertion
decodeConfigDatum = do
    let configHex =
            "d8799f9f581cf43138a5c2f37cc8c074c90a5b347d7b2b3ebf729a44b9bbdc883787581c7a3c29ca42cc2d4856682a4564c776843e8b9135cf73c3ed9e986aba581c4fd090d48fceef9df09819f58c1d8d7cbf1b3556ca8414d3865a201c581cad27a6879d211d50225f7506534bbb3c8a47e66bbe78ef800dc7b3bcff03581c642c1f7bf79ca48c0f97239fcb2f3b42b92f2548184ab394e1e1e503d8799fd8799f581caf21fa93ded7a12960b09bd1bc95d007f90513be8977ca40c97582d7ffd87a80ff1a000f4240d8799f031903e8ff1a000f42401a00200b20ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @PartialOrder.PartialOrderConfigDatum configHex)
    HUnit.assertEqual "signatory count" 4 (length $ PartialOrder.pocdSignatories datum)
    HUnit.assertEqual "required signatories" 3 (PartialOrder.pocdReqSignatores datum)
    HUnit.assertEqual "nft symbol" (hexToBuiltinByteString "642c1f7bf79ca48c0f97239fcb2f3b42b92f2548184ab394e1e1e503") (PartialOrder.pocdNftSymbol datum)
    HUnit.assertEqual "maker fee flat" 1000000 (PartialOrder.pocdMakerFeeFlat datum)
    HUnit.assertEqual "maker fee numerator" 3 (Common.rNumerator $ PartialOrder.pocdMakerFeeRatio datum)
    HUnit.assertEqual "maker fee denominator" 1000 (Common.rDenominator $ PartialOrder.pocdMakerFeeRatio datum)
    HUnit.assertEqual "taker fee" 1000000 (PartialOrder.pocdTakerFee datum)
    HUnit.assertEqual "min deposit" 2100000 (PartialOrder.pocdMinDeposit datum)

decodeOrderDatum :: HUnit.Assertion
decodeOrderDatum = do
    let orderHex =
            "d8799f581cbc399072ac81a192ed28cb90349704774dece24e85cc6d6841625f6bd8799fd8799f581cbc399072ac81a192ed28cb90349704774dece24e85cc6d6841625f6bffd8799fd8799fd8799f581c58073532d0a3987d9af950c0c6522d17e0e4c68b3418e97821dd29a1ffffffffd8799f4040ff0505d8799f581c279c909f348e533da5808898f87f9a14bb2c3dfbbacccd631d927a3f44534e454bffd8799f1908bc01ff5820826d0b1e0e6547670b12d4b4bdb85af52216ad4db8ed6c1814ba82aad2793613d87a80d87a80001a000f42401a000f4240d8799f1a000f42400000ff00ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @PartialOrder.PartialOrderDatum orderHex)
    HUnit.assertEqual "owner key" (hexToBuiltinByteString "bc399072ac81a192ed28cb90349704774dece24e85cc6d6841625f6b") (PartialOrder.podOwnerKey datum)
    HUnit.assertEqual "offered amount original" 5 (PartialOrder.podOfferedOriginalAmount datum)
    HUnit.assertEqual "offered amount remaining" 5 (PartialOrder.podOfferedAmount datum)
    HUnit.assertEqual "asked asset symbol" (hexToBuiltinByteString "279c909f348e533da5808898f87f9a14bb2c3dfbbacccd631d927a3f") (Common.acSymbol $ PartialOrder.podAskedAsset datum)
    HUnit.assertEqual "asked asset name" (hexToBuiltinByteString "534e454b") (Common.acName $ PartialOrder.podAskedAsset datum)
    HUnit.assertEqual "price numerator" 2236 (Common.rNumerator $ PartialOrder.podPrice datum)
    HUnit.assertEqual "price denominator" 1 (Common.rDenominator $ PartialOrder.podPrice datum)
    HUnit.assertEqual "partial fills" 0 (PartialOrder.podPartialFills datum)
    HUnit.assertEqual "maker fee flat" 1000000 (PartialOrder.podMakerLovelaceFlatFee datum)
    HUnit.assertEqual "taker fee flat" 1000000 (PartialOrder.podTakerLovelaceFlatFee datum)
    HUnit.assertEqual "contained payment" 0 (PartialOrder.podContainedPayment datum)
