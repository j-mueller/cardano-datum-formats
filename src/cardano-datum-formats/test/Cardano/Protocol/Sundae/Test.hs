module Cardano.Protocol.Sundae.Test (
    tests,
) where

import Cardano.Api qualified as C
import Cardano.Asset (Asset (..))
import Cardano.Data qualified as Datum
import Cardano.Protocol.Sundae.V3.Order qualified as Order
import Cardano.Protocol.Sundae.V3.Pool qualified as Pool
import Cardano.Protocol.Sundae.V4.Order qualified as OrderV4
import Cardano.Protocol.Sundae.V4.Pool qualified as PoolV4
import Cardano.Test.Utils (datumRoundTrip, fromHexRawBytes, hexToBuiltinByteString, scriptDataFromHex)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "sundae"
        [ testGroup
            "order datum"
            [ testProperty "MultisigScript" (datumRoundTrip @Order.MultisigScript)
            , testProperty "Credential" (datumRoundTrip @Order.Credential)
            , testProperty "StakingCredential" (datumRoundTrip @Order.StakingCredential)
            , testProperty "SundaeAddress" (datumRoundTrip @Order.SundaeAddress)
            , testProperty "SundaeDatum" (datumRoundTrip @Order.SundaeDatum)
            , testProperty "Destination" (datumRoundTrip @Order.Destination)
            , testProperty "StrategyAuthorization" (datumRoundTrip @Order.StrategyAuthorization)
            , testProperty "AssetAmount" (datumRoundTrip @Order.AssetAmount)
            , testProperty "Order" (datumRoundTrip @Order.Order)
            , testProperty "SundaeOrderDatum" (datumRoundTrip @Order.SundaeOrderDatum)
            , HUnit.testCase "decode swap datum from sdk vector" decodeSwapDatum
            , HUnit.testCase "decode valid order datum from sdk vector" decodeValidOrder
            ]
        , testGroup
            "pool datum"
            [ testProperty "FeeBasisPoints" (datumRoundTrip @Pool.FeeBasisPoints)
            , testProperty "ProtocolFeeAmounts" (datumRoundTrip @Pool.ProtocolFeeAmounts)
            , testProperty "SundaePoolDatum" (datumRoundTrip @Pool.SundaePoolDatum)
            , testProperty "SundaeStablePoolDatum" (datumRoundTrip @Pool.SundaeStablePoolDatum)
            , HUnit.testCase "decode v3 pool datum from sdk vector" decodeV3PoolDatum
            , HUnit.testCase "decode stableswap pool datum from sdk vector" decodeStablePoolDatum
            , HUnit.testCase "decode mainnet v3 pool datum" decodeMainnetV3PoolDatum
            , HUnit.testCase "decode mainnet stableswap pool datum" decodeMainnetStablePoolDatum
            ]
        , testGroup
            "v4 order datum"
            [ testProperty "MultisigScript" (datumRoundTrip @OrderV4.MultisigScript)
            , testProperty "Credential" (datumRoundTrip @OrderV4.Credential)
            , testProperty "StakingCredential" (datumRoundTrip @OrderV4.StakingCredential)
            , testProperty "SundaeAddress" (datumRoundTrip @OrderV4.SundaeAddress)
            , testProperty "Destination" (datumRoundTrip @OrderV4.Destination)
            , testProperty "BasicConstraint" (datumRoundTrip @OrderV4.BasicConstraint)
            , testProperty "SwapConstraint" (datumRoundTrip @OrderV4.SwapConstraint)
            , testProperty "FairnessConstraint" (datumRoundTrip @OrderV4.FairnessConstraint)
            , testProperty "FeeConstraint" (datumRoundTrip @OrderV4.FeeConstraint)
            , testProperty "OrderConfig" (datumRoundTrip @OrderV4.OrderConfig)
            , testProperty "OrderConstraint" (datumRoundTrip @OrderV4.OrderConstraint)
            , testProperty "OrderDatum" (datumRoundTrip @OrderV4.OrderDatum)
            , HUnit.testCase "decode SDK basic swap constraint" decodeV4BasicSwapConstraint
            , HUnit.testCase "decode SDK partial-fill swap constraint" decodeV4SwapConstraint
            , HUnit.testCase "build swap intent constraints" buildV4SwapIntentConstraints
            , HUnit.testCase "decode contract-generated order datum" decodeV4OrderDatum
            ]
        , testGroup
            "v4 pool datum"
            [ testProperty "Rational" (datumRoundTrip @PoolV4.Rational)
            , testProperty "ConstantSumConfig" (datumRoundTrip @PoolV4.ConstantSumConfig)
            , testProperty "AssetAmount" (datumRoundTrip @PoolV4.AssetAmount)
            , testProperty "ActionEntry" (datumRoundTrip @PoolV4.ActionEntry)
            , testProperty "ModuleState" (datumRoundTrip @PoolV4.ModuleState)
            , testProperty "PoolDatum" (datumRoundTrip @PoolV4.PoolDatum)
            , HUnit.testCase "decode SDK constant-sum config" decodeV4ConstantSumConfig
            , HUnit.testCase "decode sdk pool datum" decodeV4PoolDatum
            ]
        ]

decodeV4BasicSwapConstraint :: HUnit.Assertion
decodeV4BasicSwapConstraint = do
    let sdkHex =
            "d87b9f9f9fd8799f42aaaa41aaff1a000f4240ffff9f9fd8799f42bbbb41bbff1a000dbba0ffffff"
    constraint <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @OrderV4.BasicConstraint sdkHex)
    HUnit.assertEqual
        "basic swap"
        (OrderV4.BasicSwap [offeredAmount 1_000_000] [receivedAmount 900_000])
        constraint
    assertDatumMatchesFixture sdkHex constraint

decodeV4SwapConstraint :: HUnit.Assertion
decodeV4SwapConstraint = do
    let sdkHex =
            "d87b9fd8799f42aaaa41aaff1a000f42401a000b71b09f9fd8799f42bbbb41bbff1a000dbba0ffffff"
    constraint <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @OrderV4.SwapConstraint sdkHex)
    HUnit.assertEqual
        "partial-fill swap"
        ( OrderV4.SwapConstraint
            (Asset (hexToBuiltinByteString "aaaa") (hexToBuiltinByteString "aa"))
            (C.Quantity 1_000_000)
            (C.Quantity 750_000)
            [receivedAmount 900_000]
        )
        constraint
    assertDatumMatchesFixture sdkHex constraint

buildV4SwapIntentConstraints :: HUnit.Assertion
buildV4SwapIntentConstraints = do
    let basicHash = hexToBuiltinByteString "11"
        feeHash = hexToBuiltinByteString "22"
        constraints =
            OrderV4.swapIntentConstraints
                basicHash
                feeHash
                (offeredAmount 1_000_000)
                [receivedAmount 900_000]
    HUnit.assertEqual "module hashes" [basicHash, feeHash] (OrderV4.ocModuleHash <$> constraints)
    case constraints of
        [basicConstraint, feeConstraint] -> do
            HUnit.assertEqual
                "basic payload"
                (PlutusTx.toBuiltinData $ OrderV4.BasicSwap [offeredAmount 1_000_000] [receivedAmount 900_000])
                (OrderV4.ocData basicConstraint)
            HUnit.assertEqual
                "fee payload"
                (PlutusTx.mkConstr 0 [])
                (OrderV4.ocData feeConstraint)
        _ -> HUnit.assertFailure "expected basic and fee constraints"

offeredAmount :: Integer -> PoolV4.AssetAmount
offeredAmount amount =
    PoolV4.AssetAmount
        (Asset (hexToBuiltinByteString "aaaa") (hexToBuiltinByteString "aa"))
        (C.Quantity amount)

receivedAmount :: Integer -> PoolV4.AssetAmount
receivedAmount amount =
    PoolV4.AssetAmount
        (Asset (hexToBuiltinByteString "bbbb") (hexToBuiltinByteString "bb"))
        (C.Quantity amount)

decodeV4OrderDatum :: HUnit.Assertion
decodeV4OrderDatum = do
    -- Canonical SDK CBOR with its short mock hashes extended to valid Cardano hash lengths.
    let contractHex =
            "d8799fd8799f581caabbccdd00112233445566778899aabb000000000000000000000000ffd8799fd8799fd8799f581caabb0011223344556677889900112233000000000000000000000000ffd87a80ffd87a80ff1a0016e3601901f442aabb9f9f581d0011223344556677889900aabbccddeeff00112233445566778899aabbd8799f9f9fd8799f42aaaa41aaff1a000f4240ffff9f9fd8799f42bbbb41bbff1a000dbba0ffffffffffd87980ff"
    orderDatum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @OrderV4.OrderDatum contractHex)
    HUnit.assertEqual
        "owner"
        (OrderV4.Signature $ mkPaymentKeyHash "aabbccdd00112233445566778899aabb000000000000000000000000")
        (OrderV4.odOwner orderDatum)
    HUnit.assertEqual "service budget" (C.Quantity 1_500_000) (OrderV4.odServiceBudget orderDatum)
    HUnit.assertEqual "max per execution" (C.Quantity 500) (OrderV4.odMaxPerExecution orderDatum)
    HUnit.assertEqual "config token" (hexToBuiltinByteString "aabb") (OrderV4.odConfigToken orderDatum)
    HUnit.assertEqual "constraint count" 1 (length $ OrderV4.odConstraints orderDatum)
    HUnit.assertEqual "extension" (PlutusTx.mkConstr 0 []) (OrderV4.odExtension orderDatum)
    assertDatumMatchesFixture contractHex orderDatum

decodeV4PoolDatum :: HUnit.Assertion
decodeV4PoolDatum = do
    -- Golden CBOR from the official SDK's v4 pool datum test.
    let sdkHex =
            "d8799f9f9fd8799f42aaaa41aaff1903e8ff9fd8799f42bbbb41bbff1903e8ffff1903e8001903e844deadbeef9fd8799f1864d87a809f581c11111111111111111111111111111111111111111111111111111111581c22222222222222222222222222222222222222222222222222222222581c33333333333333333333333333333333333333333333333333333333ffffff9f9f581c1111111111111111111111111111111111111111111111111111111158201000fdabb5fe420325e8c9bf815eef8fd18f32b131bc5c9c62f568bdabc66621ff9f581c2222222222222222222222222222222222222222222222222222222244aabbccddff9f581c333333333333333333333333333333333333333333333333333333334180ffff1a004c4b40d87980ff"
    poolDatum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @PoolV4.PoolDatum sdkHex)
    HUnit.assertEqual "asset count" 2 (length $ PoolV4.pdAssets poolDatum)
    HUnit.assertEqual "total LP" (C.Quantity 1_000) (PoolV4.pdTotalLp poolDatum)
    HUnit.assertEqual "circulating LP" (C.Quantity 0) (PoolV4.pdCirculatingLp poolDatum)
    HUnit.assertEqual "preminted LP" (C.Quantity 1_000) (PoolV4.pdPremintedLp poolDatum)
    HUnit.assertEqual "identifier" (hexToBuiltinByteString "deadbeef") (PoolV4.pdIdentifier poolDatum)
    HUnit.assertEqual "action count" 1 (length $ PoolV4.pdActions poolDatum)
    HUnit.assertEqual "module state count" 3 (length $ PoolV4.pdModuleState poolDatum)
    HUnit.assertEqual "minimum surplus" (C.Quantity 5_000_000) (PoolV4.pdMinSurplus poolDatum)
    HUnit.assertEqual "extension" (PlutusTx.mkConstr 0 []) (PoolV4.pdExtension poolDatum)
    assertDatumMatchesFixture sdkHex poolDatum

decodeV4ConstantSumConfig :: HUnit.Assertion
decodeV4ConstantSumConfig = do
    let sdkHex = "d8799f9f0101ffd8799f010affd8799f0001ffd87980ff"
    config <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @PoolV4.ConstantSumConfig sdkHex)
    HUnit.assertEqual "prices" [1, 1] (PoolV4.cscPrices config)
    HUnit.assertEqual "fee" (PoolV4.Rational 1 10) (PoolV4.cscFee config)
    HUnit.assertEqual "bounty" (PoolV4.Rational 0 1) (PoolV4.cscBountyK config)
    HUnit.assertEqual "waive fee on claim" False (PoolV4.cscWaiveFeeOnClaim config)
    assertDatumMatchesFixture sdkHex config

assertDatumMatchesFixture :: (PlutusTx.ToData a) => String -> a -> HUnit.Assertion
assertDatumMatchesFixture fixtureHex value = do
    expected <- either HUnit.assertFailure pure (scriptDataFromHex fixtureHex)
    let actual = C.fromPlutusData $ PlutusTx.builtinDataToData $ PlutusTx.toBuiltinData value
    HUnit.assertEqual "re-encoded datum" expected actual

decodeSwapDatum :: HUnit.Assertion
decodeSwapDatum = do
    let expectedHex =
            "d8799fd8799f581ca933477ea168013e2b5af4a9e029e36d26738eb6dfe382e1f3eab3e2ffd8799f581c121fd22e0b57ac206fefc763f8bfa0771919f5218b40691eea4514d0ff1a000f4240d8799fd8799fd8799f581cc279a3fb3b4e62bbc78e288783b58045d4ae82a18867d8352d02775affd8799fd8799fd8799f581c121fd22e0b57ac206fefc763f8bfa0771919f5218b40691eea4514d0ffffffffd87980ffd87a9f9f40401864ff9f581cfa3eff2047fdf9293c5feef4dc85ce58097ea1c6da4845a3515351834574494e44591864ffff43d87980ff"
    orderDatum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Order.SundaeOrderDatum expectedHex)
    HUnit.assertEqual
        "pool ident"
        (Just $ hexToBuiltinByteString "a933477ea168013e2b5af4a9e029e36d26738eb6dfe382e1f3eab3e2")
        (Order.soPoolIdent orderDatum)
    HUnit.assertEqual
        "owner"
        (Order.Signature $ mkPaymentKeyHash "121fd22e0b57ac206fefc763f8bfa0771919f5218b40691eea4514d0")
        (Order.soOwner orderDatum)
    HUnit.assertEqual "max protocol fee" (C.Quantity 1_000_000) (Order.soMaxProtocolFee orderDatum)
    HUnit.assertEqual
        "destination"
        (Order.FixedDestination fixedAddress Order.NoDatum)
        (Order.soDestination orderDatum)
    HUnit.assertEqual
        "details"
        ( Order.Swap
            (Order.AssetAmount C.AdaAssetId (C.Quantity 100))
            ( Order.AssetAmount
                (C.AssetId (mkPolicyId "fa3eff2047fdf9293c5feef4dc85ce58097ea1c6da4845a351535183") (mkAssetName "74494e4459"))
                (C.Quantity 100)
            )
        )
        (Order.soDetails orderDatum)
    HUnit.assertEqual "extension" (hexToBuiltinByteString "d87980") (Order.soExtension orderDatum)
  where
    fixedAddress =
        Order.SundaeAddress
            (Order.VerificationKeyCredential $ mkPaymentKeyHash "c279a3fb3b4e62bbc78e288783b58045d4ae82a18867d8352d02775a")
            (Just $ Order.Inline $ Order.VerificationKeyCredential $ mkPaymentKeyHash "121fd22e0b57ac206fefc763f8bfa0771919f5218b40691eea4514d0")

decodeValidOrder :: HUnit.Assertion
decodeValidOrder = do
    let vectorFromSdk =
            "d8799fd8799f581ca933477ea168013e2b5af4a9e029e36d26738eb6dfe382e1f3eab3e2ffd8799f581c121fd22e0b57ac206fefc763f8bfa0771919f5218b40691eea4514d0ff1a000f4240d8799fd8799fd8799f581cc279a3fb3b4e62bbc78e288783b58045d4ae82a18867d8352d02775affd8799fd8799fd8799f581c121fd22e0b57ac206fefc763f8bfa0771919f5218b40691eea4514d0ffffffffd87980ffd87a9f9f40401864ff9f581cfa3eff2047fdf9293c5feef4dc85ce58097ea1c6da4845a3515351834574494e44591864ffff43d87980ff"
    scriptData <-
        either
            HUnit.assertFailure
            pure
            (scriptDataFromHex vectorFromSdk)
    let decoded = PlutusTx.fromBuiltinData @Order.SundaeOrderDatum $ PlutusTx.toBuiltinData $ PlutusTx.dataToBuiltinData $ C.toPlutusData scriptData
    HUnit.assertBool "should decode Sundae order datum from sdk vector" (isJust decoded)

decodeV3PoolDatum :: HUnit.Assertion
decodeV3PoolDatum = do
    let expectedHex =
            "d8799f581c82f70fd1663b2b6f4250d6054fe4cac8c815d9eef8b38f68bbe22c929f9f4040ff9f581cfa3eff2047fdf9293c5feef4dc85ce58097ea1c6da4845a3515351834574494e4459ffff1a01312d000505d87a80187b1a001e8480ff"
    poolDatum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Pool.SundaePoolDatum expectedHex)
    HUnit.assertEqual
        "identifier"
        (hexToBuiltinByteString "82f70fd1663b2b6f4250d6054fe4cac8c815d9eef8b38f68bbe22c92")
        (Pool.spdIdentifier poolDatum)
    HUnit.assertEqual "asset A" C.AdaAssetId (Pool.spdAssetA poolDatum)
    HUnit.assertEqual
        "asset B"
        (C.AssetId (mkPolicyId "fa3eff2047fdf9293c5feef4dc85ce58097ea1c6da4845a351535183") (mkAssetName "74494e4459"))
        (Pool.spdAssetB poolDatum)
    HUnit.assertEqual "circulating LP" (C.Quantity 20_000_000) (Pool.spdCirculatingLp poolDatum)
    HUnit.assertEqual "bid fee" 5 (Pool.spdBidFeesPer10Thousand poolDatum)
    HUnit.assertEqual "ask fee" 5 (Pool.spdAskFeesPer10Thousand poolDatum)
    HUnit.assertEqual "fee manager" Nothing (Pool.spdFeeManager poolDatum)
    HUnit.assertEqual "market open" 123 (Pool.spdMarketOpen poolDatum)
    HUnit.assertEqual "protocol fees" (C.Quantity 2_000_000) (Pool.spdProtocolFees poolDatum)

decodeStablePoolDatum :: HUnit.Assertion
decodeStablePoolDatum = do
    let expectedHex =
            "d8799f581c82f70fd1663b2b6f4250d6054fe4cac8c815d9eef8b38f68bbe22c929f9f4040ff9f581cfa3eff2047fdf9293c5feef4dc85ce58097ea1c6da4845a3515351834574494e4459ffff1a02625a009f0505ff9f0505ffd87a80187b9f1a001e84800000ff18c8c249022b1c8c1227a00000d87a80ff"
    poolDatum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Pool.SundaeStablePoolDatum expectedHex)
    HUnit.assertEqual
        "identifier"
        (hexToBuiltinByteString "82f70fd1663b2b6f4250d6054fe4cac8c815d9eef8b38f68bbe22c92")
        (Pool.sspdIdentifier poolDatum)
    HUnit.assertEqual "asset A" C.AdaAssetId (Pool.sspdAssetA poolDatum)
    HUnit.assertEqual
        "asset B"
        (C.AssetId (mkPolicyId "fa3eff2047fdf9293c5feef4dc85ce58097ea1c6da4845a351535183") (mkAssetName "74494e4459"))
        (Pool.sspdAssetB poolDatum)
    HUnit.assertEqual "circulating LP" (C.Quantity 40_000_000) (Pool.sspdCirculatingLp poolDatum)
    HUnit.assertEqual "LP fees" (Pool.FeeBasisPoints 5 5) (Pool.sspdLpFeeBasisPoints poolDatum)
    HUnit.assertEqual "protocol fee basis points" (Pool.FeeBasisPoints 5 5) (Pool.sspdProtocolFeeBasisPoints poolDatum)
    HUnit.assertEqual "fee manager" Nothing (Pool.sspdFeeManager poolDatum)
    HUnit.assertEqual "market open" 123 (Pool.sspdMarketOpen poolDatum)
    HUnit.assertEqual
        "protocol fees"
        (Pool.ProtocolFeeAmounts (C.Quantity 2_000_000) (C.Quantity 0) (C.Quantity 0))
        (Pool.sspdProtocolFees poolDatum)
    HUnit.assertEqual "linear amplification" 200 (Pool.sspdLinearAmplification poolDatum)
    HUnit.assertEqual "sum invariant" 40_000_000_000_000_000_000 (Pool.sspdSumInvariant poolDatum)
    HUnit.assertEqual "linear amplification manager" Nothing (Pool.sspdLinearAmplificationManager poolDatum)

decodeMainnetV3PoolDatum :: HUnit.Assertion
decodeMainnetV3PoolDatum = do
    let mainnetHex =
            "d8799f581c354cb5f081914758da968cd21d84201035738ba0829e843b50202a829f9f4040ff9f581c437345349626487273a70dc7b0f211d58e8766760e5177f09890fef34342474fffff00181e181ed87a80001a0138fd03ff"
    poolDatum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Pool.SundaePoolDatum mainnetHex)
    HUnit.assertEqual "asset A" C.AdaAssetId (Pool.spdAssetA poolDatum)
    HUnit.assertEqual
        "asset B"
        (C.AssetId (mkPolicyId "437345349626487273a70dc7b0f211d58e8766760e5177f09890fef3") (mkAssetName "42474f"))
        (Pool.spdAssetB poolDatum)
    HUnit.assertEqual "bid fee" 30 (Pool.spdBidFeesPer10Thousand poolDatum)
    HUnit.assertEqual "ask fee" 30 (Pool.spdAskFeesPer10Thousand poolDatum)
    HUnit.assertEqual "fee manager" Nothing (Pool.spdFeeManager poolDatum)

decodeMainnetStablePoolDatum :: HUnit.Assertion
decodeMainnetStablePoolDatum = do
    let mainnetHex =
            "d8799f581cd7c7a7db47ab71ef07f0aa65e6b0bcf9409977c183e85fe6f0a5feb69f9f581c1f3aec8bfe7ea4fe14c5f121e2a92e301afe414147860d557cac7e34455553444378ff9f581cc48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad480014df105553444dffff1b000005208184ecc59f0505ff9f0101ffd87a80009f1abd53327b1a0ded0edb1a0b26f03fff1901f4c24b04abee96e94f13cd70f22fd87a80ff"
    poolDatum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Pool.SundaeStablePoolDatum mainnetHex)
    HUnit.assertEqual
        "asset A"
        (C.AssetId (mkPolicyId "1f3aec8bfe7ea4fe14c5f121e2a92e301afe414147860d557cac7e34") (mkAssetName "5553444378"))
        (Pool.sspdAssetA poolDatum)
    HUnit.assertEqual
        "asset B"
        (C.AssetId (mkPolicyId "c48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad") (mkAssetName "0014df105553444d"))
        (Pool.sspdAssetB poolDatum)
    HUnit.assertEqual "LP fees" (Pool.FeeBasisPoints 5 5) (Pool.sspdLpFeeBasisPoints poolDatum)
    HUnit.assertEqual "protocol fee basis points" (Pool.FeeBasisPoints 1 1) (Pool.sspdProtocolFeeBasisPoints poolDatum)
    HUnit.assertEqual "linear amplification manager" Nothing (Pool.sspdLinearAmplificationManager poolDatum)

mkPaymentKeyHash :: String -> C.Hash C.PaymentKey
mkPaymentKeyHash hex = fromHexRawBytes (C.proxyToAsType $ Proxy @(C.Hash C.PaymentKey)) hex

mkPolicyId :: String -> C.PolicyId
mkPolicyId hex = fromHexRawBytes C.AsPolicyId hex

mkAssetName :: String -> C.AssetName
mkAssetName hex = fromHexRawBytes C.AsAssetName hex
