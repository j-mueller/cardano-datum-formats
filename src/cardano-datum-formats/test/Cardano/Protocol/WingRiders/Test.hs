module Cardano.Protocol.WingRiders.Test (
    tests,
) where

import Cardano.Data qualified as Datum
import Cardano.Protocol.WingRiders.Pool qualified as Pool
import Cardano.Test.Utils (datumRoundTrip, hexToBuiltinByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "wingriders"
        [ testGroup
            "datum roundtrips"
            [ testProperty "FeeFrom" (datumRoundTrip @Pool.FeeFrom)
            , testProperty "WingRidersPoolDatum" (datumRoundTrip @Pool.WingRidersPoolDatum)
            ]
        , testGroup
            "mainnet decode"
            [ HUnit.testCase "decode live pool datum" decodeLivePoolDatum
            ]
        ]

decodeLivePoolDatum :: HUnit.Assertion
decodeLivePoolDatum = do
    let poolHex = "d8799f4040581cc0ee29a85b13209423b10447d3c2e6a50641a15c57770e27cb9d50734a57696e675269646572730000d879804040000001011901f4582075945dd22934cd6b373b8943c14ab7aa8c6a2d1de826b71f88bc1aa663ce6bbdff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Pool.WingRidersPoolDatum poolHex)
    HUnit.assertEqual "asset A policy" (hexToBuiltinByteString "") (Pool.wrpdAssetAPolicyId datum)
    HUnit.assertEqual "asset A name" (hexToBuiltinByteString "") (Pool.wrpdAssetAName datum)
    HUnit.assertEqual "asset B policy" (hexToBuiltinByteString "c0ee29a85b13209423b10447d3c2e6a50641a15c57770e27cb9d5073") (Pool.wrpdAssetBPolicyId datum)
    HUnit.assertEqual "asset B name" (hexToBuiltinByteString "57696e67526964657273") (Pool.wrpdAssetBName datum)
    HUnit.assertEqual "treasury A" 0 (Pool.wrpdTreasuryA datum)
    HUnit.assertEqual "treasury B" 0 (Pool.wrpdTreasuryB datum)
    HUnit.assertEqual "fee from" Pool.InputToken (Pool.wrpdFeeFrom datum)
    HUnit.assertEqual "treasury authority policy" (hexToBuiltinByteString "") (Pool.wrpdTreasuryAuthorityPolicyId datum)
    HUnit.assertEqual "treasury authority name" (hexToBuiltinByteString "") (Pool.wrpdTreasuryAuthorityAssetName datum)
    HUnit.assertEqual "treasury fee A->B" 0 (Pool.wrpdTreasuryFeePointsAToB datum)
    HUnit.assertEqual "treasury fee B->A" 0 (Pool.wrpdTreasuryFeePointsBToA datum)
    HUnit.assertEqual "swap fee A->B" 1 (Pool.wrpdSwapFeePointsAToB datum)
    HUnit.assertEqual "swap fee B->A" 1 (Pool.wrpdSwapFeePointsBToA datum)
    HUnit.assertEqual "fee basis" 500 (Pool.wrpdFeeBasis datum)
    HUnit.assertEqual "shares asset name" (hexToBuiltinByteString "75945dd22934cd6b373b8943c14ab7aa8c6a2d1de826b71f88bc1aa663ce6bbd") (Pool.wrpdSharesAssetName datum)
