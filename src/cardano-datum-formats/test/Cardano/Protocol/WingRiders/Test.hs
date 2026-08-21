module Cardano.Protocol.WingRiders.Test (
    tests,
) where

import Cardano.Data qualified as Datum
import Cardano.Protocol.WingRiders.Pool qualified as Pool
import Cardano.Protocol.WingRiders.V2.Pool qualified as V2Pool
import Cardano.Protocol.WingRiders.V2.Request qualified as V2Request
import Cardano.Test.Utils (datumRoundTrip, hexToBuiltinByteString)
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
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
            , testProperty "V2 ConstantProductPoolDatum" (datumRoundTrip @V2Pool.ConstantProductPoolDatum)
            , testProperty "V2 StableswapPoolDatum" (datumRoundTrip @V2Pool.StableswapPoolDatum)
            , testProperty "V2 PoolDatum ConstantProduct" (datumRoundTrip @(V2Pool.PoolDatum V2Pool.ConstantProductPoolDatum))
            , testProperty "V2 PoolDatum Stableswap" (datumRoundTrip @(V2Pool.PoolDatum V2Pool.StableswapPoolDatum))
            , testProperty "V2 RequestDatum" (datumRoundTrip @V2Request.RequestDatum)
            ]
        , testGroup
            "mainnet decode"
            [ HUnit.testCase "decode live pool datum" decodeLivePoolDatum
            , HUnit.testCase "decode live V2 constant-product pool datum" decodeLiveV2ConstantProductPoolDatum
            , HUnit.testCase "decode live V2 stableswap pool datum" decodeLiveV2StableswapPoolDatum
            , HUnit.testCase "encode live V2 swap request layout" encodeLiveV2SwapRequest
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

decodeLiveV2ConstantProductPoolDatum :: HUnit.Assertion
decodeLiveV2ConstantProductPoolDatum = do
    let poolHex = "d8799f581cc134d839a64a5dfb9b155869ef3f34280751a622f69958baa8ffd29c4040581cc48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad480014df105553444d140500001927101a001e84801b000001a0246afb101a2335c3611a07bfc82100000000d87a80d87a80d87980ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @(V2Pool.PoolDatum V2Pool.ConstantProductPoolDatum) poolHex)
    HUnit.assertEqual "request validator" (hexToBuiltinByteString "c134d839a64a5dfb9b155869ef3f34280751a622f69958baa8ffd29c") (V2Pool.wrpdRequestValidatorHash datum)
    HUnit.assertEqual "asset A policy" (hexToBuiltinByteString "") (V2Pool.wrpdAssetASymbol datum)
    HUnit.assertEqual "asset B policy" (hexToBuiltinByteString "c48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad") (V2Pool.wrpdAssetBSymbol datum)
    HUnit.assertEqual "asset B name" (hexToBuiltinByteString "0014df105553444d") (V2Pool.wrpdAssetBToken datum)
    HUnit.assertEqual "swap fee" 20 (V2Pool.wrpdSwapFeeInBasis datum)
    HUnit.assertEqual "protocol fee" 5 (V2Pool.wrpdProtocolFeeInBasis datum)
    HUnit.assertEqual "fee basis" 10_000 (V2Pool.wrpdFeeBasis datum)
    HUnit.assertEqual "agent fee" 2_000_000 (V2Pool.wrpdAgentFeeAda datum)
    HUnit.assertEqual "constant-product specifics" V2Pool.ConstantProductPoolDatum (V2Pool.wrpdPoolSpecifics datum)

decodeLiveV2StableswapPoolDatum :: HUnit.Assertion
decodeLiveV2StableswapPoolDatum = do
    let poolHex = "d8799f581c23680ea6701b56f2c12ae79d8af94fd36f509b7b007029c7ce114840581c8db269c3ec630e06ae29f74bc39edd1f87c819f1056206e879a1cd614c446a65644d6963726f555344581cc48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad480014df105553444d050100001927101a001e84801b000001a00fdd27b81a0006867d1a0009167600000000d87a80d87a80d8799f1a1a6227050101ffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @(V2Pool.PoolDatum V2Pool.StableswapPoolDatum) poolHex)
    HUnit.assertEqual "swap fee" 5 (V2Pool.wrpdSwapFeeInBasis datum)
    HUnit.assertEqual "protocol fee" 1 (V2Pool.wrpdProtocolFeeInBasis datum)
    HUnit.assertEqual "stable specifics" (V2Pool.StableswapPoolDatum 442_640_133 1 1) (V2Pool.wrpdPoolSpecifics datum)

encodeLiveV2SwapRequest :: HUnit.Assertion
encodeLiveV2SwapRequest = do
    let owner =
            V2Request.Address
                (V2Request.VerificationKeyCredential $ hexToBuiltinByteString "aae04f52cb97215557343539ef77474fcbd0157664135c1fe915c89e")
                Nothing
        datum =
            V2Request.RequestDatum
                { V2Request.wrrdOil = 2_000_000
                , V2Request.wrrdBeneficiary = owner
                , V2Request.wrrdOwnerAddress = owner
                , V2Request.wrrdCompensationDatum = Builtins.mkList []
                , V2Request.wrrdCompensationDatumType = V2Request.NoDatum
                , V2Request.wrrdDeadline = 1_787_344_633_822
                , V2Request.wrrdAssetASymbol = hexToBuiltinByteString ""
                , V2Request.wrrdAssetAToken = hexToBuiltinByteString ""
                , V2Request.wrrdAssetBSymbol = hexToBuiltinByteString "c48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad"
                , V2Request.wrrdAssetBToken = hexToBuiltinByteString "0014df105553444d"
                , V2Request.wrrdAction = V2Request.Swap V2Request.SwapAToB 2_414_428_561
                , V2Request.wrrdAScale = 1
                , V2Request.wrrdBScale = 1
                }
        expected =
            Builtins.mkConstr
                0
                [ Builtins.mkI 2_000_000
                , PlutusTx.toBuiltinData owner
                , PlutusTx.toBuiltinData owner
                , Builtins.mkList []
                , Builtins.mkConstr 0 []
                , Builtins.mkI 1_787_344_633_822
                , Builtins.mkB (hexToBuiltinByteString "")
                , Builtins.mkB (hexToBuiltinByteString "")
                , Builtins.mkB (hexToBuiltinByteString "c48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad")
                , Builtins.mkB (hexToBuiltinByteString "0014df105553444d")
                , Builtins.mkConstr 0 [Builtins.mkConstr 0 [], Builtins.mkI 2_414_428_561]
                , Builtins.mkI 1
                , Builtins.mkI 1
                ]
    HUnit.assertEqual "live request datum" expected (PlutusTx.toBuiltinData datum)
