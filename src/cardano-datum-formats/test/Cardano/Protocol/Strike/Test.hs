module Cardano.Protocol.Strike.Test (
    tests,
) where

import Cardano.Asset qualified as Asset
import Cardano.Data qualified as Datum
import Cardano.Protocol.Strike.Common qualified as Common
import Cardano.Protocol.Strike.Order qualified as Order
import Cardano.Protocol.Strike.Pool qualified as Pool
import Cardano.Protocol.Strike.Position qualified as Position
import Cardano.Protocol.Strike.Settings qualified as Settings
import Cardano.Test.Utils (datumRoundTrip, hexToBuiltinByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "strike"
        [ testGroup
            "common"
            [ testProperty "AddressHash" (datumRoundTrip @Common.AddressHash)
            , testProperty "ScriptHash" (datumRoundTrip @Common.ScriptHash)
            , testProperty "PositionSide" (datumRoundTrip @Common.PositionSide)
            , testProperty "OpenPositionType" (datumRoundTrip @Common.OpenPositionType)
            ]
        , testGroup
            "datum roundtrips"
            [ testProperty "PositionDatum" (datumRoundTrip @Position.PositionDatum)
            , testProperty "OrderAction" (datumRoundTrip @Order.OrderAction)
            , testProperty "OrderDatum" (datumRoundTrip @Order.OrderDatum)
            , testProperty "PoolDatum" (datumRoundTrip @Pool.PoolDatum)
            , testProperty "SettingsDatum" (datumRoundTrip @Settings.SettingsDatum)
            ]
        , testGroup
            "mainnet decode"
            [ HUnit.testCase "decode live ADA pool datum" decodeAdaPoolDatum
            , HUnit.testCase "decode live SNEK pool datum" decodeSnekPoolDatum
            , HUnit.testCase "decode live settings datum" decodeSettingsDatum
            , HUnit.testCase "decode live provide liquidity order datum" decodeProvideLiquidityOrderDatum
            ]
        ]

decodeAdaPoolDatum :: HUnit.Assertion
decodeAdaPoolDatum = do
    let poolHex =
            "d8799fd8799f4040ffd8799f581c9df6c4379fa196a4a965badd96cfae2831f4e02748bfcb66c8fcde404e535452494b455f504552505f4c50ff1b000004a6b37c143e1b00000291290437bb1b0000011e83f7c5e0581cc79fb04de1f01568c867c1f4c6c631abf4094156ab00432b8f920536ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Pool.PoolDatum poolHex)
    HUnit.assertEqual "underlying policy" (hexToBuiltinByteString "") (Asset.aPolicyId $ Pool.pdUnderlyingAsset datum)
    HUnit.assertEqual "lp asset policy" (hexToBuiltinByteString "9df6c4379fa196a4a965badd96cfae2831f4e02748bfcb66c8fcde40") (Asset.aPolicyId $ Pool.pdLpAsset datum)
    HUnit.assertEqual "liquidity total asset amount" 5114022335550 (Pool.pdLiquidityTotalAssetAmount datum)
    HUnit.assertEqual "liquidity total lp minted" 2822481655739 (Pool.pdLiquidityTotalLpMinted datum)
    HUnit.assertEqual "total lended amount" 1230574700000 (Pool.pdTotalLendedAmount datum)
    HUnit.assertEqual "batcher license" (hexToBuiltinByteString "c79fb04de1f01568c867c1f4c6c631abf4094156ab00432b8f920536") (Pool.pdBatcherLicense datum)

decodeSnekPoolDatum :: HUnit.Assertion
decodeSnekPoolDatum = do
    let poolHex =
            "d8799fd8799f581c279c909f348e533da5808898f87f9a14bb2c3dfbbacccd631d927a3f44534e454bffd8799f581c91206651a11c64ad878968150d8b2d8b970dff46208c66b42a9df90a4c534e454b5f504552505f4c50ff1a03c11d171a03765ff51902bc581c5f31580750e967e5fe4a4b9d544f794d2f5b7eee94f9411b3e2a41c6ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Pool.PoolDatum poolHex)
    HUnit.assertEqual "underlying policy" (hexToBuiltinByteString "279c909f348e533da5808898f87f9a14bb2c3dfbbacccd631d927a3f") (Asset.aPolicyId $ Pool.pdUnderlyingAsset datum)
    HUnit.assertEqual "underlying asset name" (hexToBuiltinByteString "534e454b") (Asset.aTokenName $ Pool.pdUnderlyingAsset datum)
    HUnit.assertEqual "lp asset policy" (hexToBuiltinByteString "91206651a11c64ad878968150d8b2d8b970dff46208c66b42a9df90a") (Asset.aPolicyId $ Pool.pdLpAsset datum)
    HUnit.assertEqual "liquidity total asset amount" 62987543 (Pool.pdLiquidityTotalAssetAmount datum)
    HUnit.assertEqual "liquidity total lp minted" 58089461 (Pool.pdLiquidityTotalLpMinted datum)
    HUnit.assertEqual "total lended amount" 700 (Pool.pdTotalLendedAmount datum)
    HUnit.assertEqual "batcher license" (hexToBuiltinByteString "5f31580750e967e5fe4a4b9d544f794d2f5b7eee94f9411b3e2a41c6") (Pool.pdBatcherLicense datum)

decodeSettingsDatum :: HUnit.Assertion
decodeSettingsDatum = do
    let settingsHex =
            "d8799f0318281b006a94d74f4300001b0000002e90edd0000414ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Settings.SettingsDatum settingsHex)
    HUnit.assertEqual "interest rate" 3 (Settings.sdInterestRate datum)
    HUnit.assertEqual "max leverage factor" 40 (Settings.sdMaxLeverageFactor datum)
    HUnit.assertEqual "max position usd value" 30000000000000000 (Settings.sdMaxPositionUsdValue datum)
    HUnit.assertEqual "min position usd value" 200000000000 (Settings.sdMinPositionUsdValue datum)
    HUnit.assertEqual "maintain margin amount" 4 (Settings.sdMaintainMarginAmount datum)
    HUnit.assertEqual "opening fee basis points" 20 (Settings.sdOpeningFeeBasisPoints datum)

decodeProvideLiquidityOrderDatum :: HUnit.Assertion
decodeProvideLiquidityOrderDatum = do
    let orderHex =
            "d8799fd87c9f581cf260bea3af56721559f94b82c41610a3131eb43bac4b576c90494ea1d8799f581c1aabcb52a9ea7db46dfcfc63a71ab87937512bd130b50d62ea443594ffd8799f4040ffffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Order.OrderDatum orderHex)
    case Order.odAction datum of
        Order.ProvideLiquidityOrder ownerPkh (Just ownerStakeKey) liquidityAsset -> do
            HUnit.assertEqual "owner pkh" (hexToBuiltinByteString "f260bea3af56721559f94b82c41610a3131eb43bac4b576c90494ea1") (Common.getAddressHash ownerPkh)
            HUnit.assertEqual "owner stake key" (hexToBuiltinByteString "1aabcb52a9ea7db46dfcfc63a71ab87937512bd130b50d62ea443594") (Common.getAddressHash ownerStakeKey)
            HUnit.assertEqual "liquidity asset is ADA" (hexToBuiltinByteString "") (Asset.aPolicyId liquidityAsset)
        _ -> HUnit.assertFailure "expected ProvideLiquidityOrder with stake key"
