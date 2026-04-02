module Cardano.Protocol.Pulse.Test (
    tests,
) where

import Cardano.Address.Aiken qualified as Aiken
import Cardano.Api qualified as C
import Cardano.Data qualified as Datum
import Cardano.Protocol.Pulse.V1.Common qualified as CommonV1
import Cardano.Protocol.Pulse.V1.Market qualified as MarketV1
import Cardano.Protocol.Pulse.V1.MarketInfo qualified as MarketInfoV1
import Cardano.Protocol.Pulse.V1.Oracle qualified as OracleV1
import Cardano.Protocol.Pulse.V1.Order qualified as OrderV1
import Cardano.Protocol.Pulse.V1.SYVault qualified as SYVaultV1
import Cardano.Protocol.Pulse.V1.YTStake qualified as YTStakeV1
import Cardano.Protocol.Pulse.V2.Common qualified as CommonV2
import Cardano.Protocol.Pulse.V2.Market qualified as MarketV2
import Cardano.Protocol.Pulse.V2.MarketInfo qualified as MarketInfoV2
import Cardano.Protocol.Pulse.V2.Oracle qualified as OracleV2
import Cardano.Protocol.Pulse.V2.Order qualified as OrderV2
import Cardano.Protocol.Pulse.V2.SYVault qualified as SYVaultV2
import Cardano.Protocol.Pulse.V2.YTStake qualified as YTStakeV2
import Cardano.Test.Utils (datumRoundTrip, fromHexRawBytes, hexToBuiltinByteString)
import Cardano.Transaction.OutputReference qualified as OutputReference
import Data.Proxy (Proxy (..))
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "pulse"
        [ testGroup
            "v1 common"
            [ testProperty "Aiken Credential" (datumRoundTrip @Aiken.Credential)
            , testProperty "Aiken StakeCredential" (datumRoundTrip @Aiken.StakeCredential)
            , testProperty "Aiken Address" (datumRoundTrip @Aiken.AikenAddress)
            , testProperty "OutputReference" (datumRoundTrip @OutputReference.OutputReference)
            , testProperty "PubKeyHash" (datumRoundTrip @CommonV1.PubKeyHash)
            ]
        , testGroup
            "v2 common"
            [ testProperty "PubKeyHash" (datumRoundTrip @CommonV2.PubKeyHash)
            ]
        , testGroup
            "v1 datum roundtrips"
            [ testProperty "MarketInfoDatum" (datumRoundTrip @MarketInfoV1.MarketInfoDatum)
            , testProperty "MarketDatum" (datumRoundTrip @MarketV1.MarketDatum)
            , testProperty "SYVaultDatum" (datumRoundTrip @SYVaultV1.SYVaultDatum)
            , testProperty "OracleDatum" (datumRoundTrip @OracleV1.OracleDatum)
            , testProperty "YTStakeDatum" (datumRoundTrip @YTStakeV1.YTStakeDatum)
            , testProperty "OrderDatum" (datumRoundTrip @OrderV1.OrderDatum)
            ]
        , testGroup
            "v2 datum roundtrips"
            [ testProperty "MarketInfoDatum" (datumRoundTrip @MarketInfoV2.MarketInfoDatum)
            , testProperty "MarketDatum" (datumRoundTrip @MarketV2.MarketDatum)
            , testProperty "SYVaultDatum" (datumRoundTrip @SYVaultV2.SYVaultDatum)
            , testProperty "OracleDatum" (datumRoundTrip @OracleV2.OracleDatum)
            , testProperty "YTStakeDatum" (datumRoundTrip @YTStakeV2.YTStakeDatum)
            , testProperty "OrderDatum" (datumRoundTrip @OrderV2.OrderDatum)
            ]
        , testGroup
            "v1 mainnet decode"
            [ HUnit.testCase "decode market info datum" decodeV1MarketInfoDatum
            , HUnit.testCase "decode market datum" decodeV1MarketDatum
            , HUnit.testCase "decode sy vault datum" decodeV1SYVaultDatum
            , HUnit.testCase "decode oracle datum" decodeV1OracleDatum
            , HUnit.testCase "decode yt stake datum" decodeV1YTStakeDatum
            , HUnit.testCase "decode order datum" decodeV1OrderDatum
            ]
        , testGroup
            "v2 structure"
            [ HUnit.testCase "market info locked flag roundtrips" v2MarketInfoLockedFlag
            , HUnit.testCase "sy vault includes reward sy" v2SYVaultIncludesRewardSy
            , HUnit.testCase "order add sy uses new constructor" v2OrderAddSyUsesNewConstructor
            ]
        ]

decodeV1MarketInfoDatum :: HUnit.Assertion
decodeV1MarketInfoDatum = do
    let marketInfoHex =
            "d8799fd8799f58201348ace5f4c75b56140c368d674d484291fff3c7604b06d96ed80501ed693fbb00ffd8799f581c91206651a11c64ad878968150d8b2d8b970dff46208c66b42a9df90a4c534e454b5f504552505f4c50ff1903e8530014df1050542d534e454b5f504552505f4c50530014df1059542d534e454b5f504552505f4c501b0000019c8c0a8aa01b00000001cf7c5800192710581c399b00074539d5c39c9d622e9afcfd1756e92f4ade0910df57a8bd39581cc3e0b44426e524ce86b5b1ae23c975aa7ae12bacf6b52dfd727c02d0581cfebe2c972dfbc6d4deb5a2a21409a2c5397fa440ff9307303d2d218a581c916d2114608f04cdf910f2e6655cf76da3c2e95d8a2bd8997dc32b35581cff0048fdba12c321bfc42d2ee9925d0d380ed26aa9f9bb2b0f30b2a1581c0c3b71e6468510341c813626cfa4d008d0b0c50d6ffcea188878e865d8799f581c14cfea925344e8f1c96985c8c1cf2e403e759b6d4113d52b1f996bf9530014df104c502d534e454b5f504552505f4c50ffd8799f581c95ff8558aeecf27e50871032069d250fce16561d820739bd6a7276f7581b535452494b455f4c505f534e454b5f334d5f30305f4f5241434c45ff1a004c4b4019cf3719373e1864194c2c18321901f4190bb8ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @MarketInfoV1.MarketInfoDatum marketInfoHex)
    HUnit.assertEqual "min liquidity" 1000 (MarketInfoV1.midMinLiquidity datum)
    HUnit.assertEqual "pt token name" (hexToBuiltinByteString "0014df1050542d534e454b5f504552505f4c50") (MarketInfoV1.midPtTokenName datum)
    HUnit.assertEqual "order script hash" (hexToBuiltinByteString "c3e0b44426e524ce86b5b1ae23c975aa7ae12bacf6b52dfd727c02d0") (MarketInfoV1.midOrderScriptHash datum)
    HUnit.assertEqual "sy script hash" (hexToBuiltinByteString "febe2c972dfbc6d4deb5a2a21409a2c5397fa440ff9307303d2d218a") (MarketInfoV1.midSyScriptHash datum)
    HUnit.assertEqual "yt stake script hash" (hexToBuiltinByteString "916d2114608f04cdf910f2e6655cf76da3c2e95d8a2bd8997dc32b35") (MarketInfoV1.midYtStakeScriptHash datum)

decodeV1MarketDatum :: HUnit.Assertion
decodeV1MarketDatum = do
    let marketHex =
            "d8799fd8799f58201348ace5f4c75b56140c368d674d484291fff3c7604b06d96ed80501ed693fbb00ff1a0005c7251a00040eba1a0003536c1903e8ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @MarketV1.MarketDatum marketHex)
    HUnit.assertEqual "total sy" 378661 (MarketV1.mdTotalSy datum)
    HUnit.assertEqual "total pt" 265914 (MarketV1.mdTotalPt datum)
    HUnit.assertEqual "total lp" 217964 (MarketV1.mdTotalLp datum)
    HUnit.assertEqual "reserved lp" 1000 (MarketV1.mdReservedLp datum)

decodeV1SYVaultDatum :: HUnit.Assertion
decodeV1SYVaultDatum = do
    let syVaultHex =
            "d8799fd8799f58201348ace5f4c75b56140c368d674d484291fff3c7604b06d96ed80501ed693fbb00ff1a00052257ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @SYVaultV1.SYVaultDatum syVaultHex)
    HUnit.assertEqual "market tx id" (hexToBuiltinByteString "1348ace5f4c75b56140c368d674d484291fff3c7604b06d96ed80501ed693fbb") (OutputReference.orTransactionId $ SYVaultV1.svdMarketId datum)
    HUnit.assertEqual "market output index" 0 (OutputReference.orOutputIndex $ SYVaultV1.svdMarketId datum)
    HUnit.assertEqual "total sy" 336471 (SYVaultV1.svdTotalSy datum)

decodeV1OracleDatum :: HUnit.Assertion
decodeV1OracleDatum = do
    let oracleHex =
            "d8799f1950101927101b0000019adc8e7493ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @OracleV1.OracleDatum oracleHex)
    HUnit.assertEqual "py index" 20496 (OracleV1.odPyIndex datum)
    HUnit.assertEqual "base" 10000 (OracleV1.odBased datum)
    HUnit.assertEqual "updated at" 1764636914835 (OracleV1.odUpdatedAt datum)

decodeV1YTStakeDatum :: HUnit.Assertion
decodeV1YTStakeDatum = do
    let ytStakeHex =
            "d8799fd8799f58201348ace5f4c75b56140c368d674d484291fff3c7604b06d96ed80501ed693fbb00ffd8799f5820c11d8e6afde8c9b0d81ee76025d56d8dcf33acbc69a329ff45042fb169db078b00ff581c233c9e7bc10c8bd5104f4a8c8eca171a14904a3452877e6ae204dea60000195010ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @YTStakeV1.YTStakeDatum ytStakeHex)
    HUnit.assertEqual "market tx id" (hexToBuiltinByteString "1348ace5f4c75b56140c368d674d484291fff3c7604b06d96ed80501ed693fbb") (OutputReference.orTransactionId $ YTStakeV1.ysdMarketId datum)
    HUnit.assertEqual "market output index" 0 (OutputReference.orOutputIndex $ YTStakeV1.ysdMarketId datum)
    HUnit.assertEqual "stake tx id" (hexToBuiltinByteString "c11d8e6afde8c9b0d81ee76025d56d8dcf33acbc69a329ff45042fb169db078b") (OutputReference.orTransactionId $ YTStakeV1.ysdStakeId datum)
    HUnit.assertEqual "stake output index" 0 (OutputReference.orOutputIndex $ YTStakeV1.ysdStakeId datum)
    HUnit.assertEqual "owner pkh" (hexToBuiltinByteString "233c9e7bc10c8bd5104f4a8c8eca171a14904a3452877e6ae204dea6") (PlutusTx.toBuiltin $ C.serialiseToRawBytes $ CommonV1.getPubKeyHash $ YTStakeV1.ysdOwnerPkh datum)
    HUnit.assertEqual "staked yt amount" 0 (YTStakeV1.ysdStakedYtAmount datum)
    HUnit.assertEqual "unclaimed sy amount" 0 (YTStakeV1.ysdUnclaimedSyAmount datum)
    HUnit.assertEqual "py index" 20496 (YTStakeV1.ysdPyIndex datum)

decodeV1OrderDatum :: HUnit.Assertion
decodeV1OrderDatum = do
    let orderHex =
            "d87c9f581cd4c35258ed54c3527e0078fa33871e986db11854d9cb4f779d89e604d8799fd8799f581cd4c35258ed54c3527e0078fa33871e986db11854d9cb4f779d89e604ffd8799fd8799fd8799f581ccf9eaf5aa407e4b84e1dfe67fe5eae41c1c5a17de12335354f391120ffffffffd87a80ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @OrderV1.OrderDatum orderHex)
    case datum of
        OrderV1.OMintLP{} -> pure ()
        _ -> HUnit.assertFailure "expected OMintLP"

v2MarketInfoLockedFlag :: HUnit.Assertion
v2MarketInfoLockedFlag = do
    let datum =
            MarketInfoV2.MarketInfoDatum
                { midMarketId = OutputReference.OutputReference (hexToBuiltinByteString $ replicate 64 '0') 0
                , midUnderlyingAsset = MarketInfoV1.midUnderlyingAsset baselineV1MarketInfo
                , midMinLiquidity = 1
                , midPtTokenName = hexToBuiltinByteString ""
                , midYtTokenName = hexToBuiltinByteString "01"
                , midExpiry = 2
                , midDuration = 3
                , midMultiplier = 4
                , midBatcherPolicyId = hexToBuiltinByteString "aa"
                , midOrderScriptHash = hexToBuiltinByteString "bb"
                , midSyScriptHash = hexToBuiltinByteString "cc"
                , midYtStakeScriptHash = hexToBuiltinByteString "dd"
                , midYtStakeAuthTokenPolicyId = hexToBuiltinByteString "ee"
                , midYieldTokenizationPolicyId = hexToBuiltinByteString "ff"
                , midLpTokenAsset = MarketInfoV1.midLpTokenAsset baselineV1MarketInfo
                , midOracleNft = MarketInfoV1.midOracleNft baselineV1MarketInfo
                , midEnvelopeAmount = 5
                , midScalarRoot = 6
                , midInitialAnchor = 7
                , midLnFeeRateRoot = 8
                , midInitPyIndex = 9
                , midLnBorrowFeeRate = 10
                , midYtRewardFeeRate = 11
                , midLpRevenueFeeRate = 12
                , midLocked = True
                }
    case Datum.withConstr (PlutusTx.toBuiltinData datum) Just of
        Just (0, fields) -> do
            HUnit.assertEqual "field count" 25 (length fields)
            locked <- maybe (HUnit.assertFailure "locked field should decode") pure (PlutusTx.fromBuiltinData @Bool $ last fields)
            HUnit.assertBool "locked should encode as true" locked
        _ -> HUnit.assertFailure "expected constructor 0"

v2SYVaultIncludesRewardSy :: HUnit.Assertion
v2SYVaultIncludesRewardSy = do
    let datum =
            SYVaultV2.SYVaultDatum
                { svdMarketId = OutputReference.OutputReference (hexToBuiltinByteString $ replicate 64 '1') 1
                , svdTotalSy = 42
                , svdRewardSy = 7
                }
    case Datum.withConstr (PlutusTx.toBuiltinData datum) Just of
        Just (0, [_, totalSy, rewardSy]) -> do
            HUnit.assertEqual "total sy" (Just 42) (Datum.getI totalSy)
            HUnit.assertEqual "reward sy" (Just 7) (Datum.getI rewardSy)
        _ -> HUnit.assertFailure "expected constructor 0 with three fields"

v2OrderAddSyUsesNewConstructor :: HUnit.Assertion
v2OrderAddSyUsesNewConstructor = do
    let datum = OrderV2.OAddSY (CommonV2.PubKeyHash $ mkPaymentKeyHash "d4c35258ed54c3527e0078fa33871e986db11854d9cb4f779d89e604")
    case Datum.withConstr (PlutusTx.toBuiltinData datum) Just of
        Just (11, [ownerPkhData]) ->
            case PlutusTx.fromBuiltinData @CommonV2.PubKeyHash ownerPkhData of
                Just ownerPkh ->
                    HUnit.assertEqual
                        "owner pkh"
                        (hexToBuiltinByteString "d4c35258ed54c3527e0078fa33871e986db11854d9cb4f779d89e604")
                        (PlutusTx.toBuiltin $ C.serialiseToRawBytes $ CommonV2.getPubKeyHash ownerPkh)
                Nothing -> HUnit.assertFailure "owner pkh should decode"
        _ -> HUnit.assertFailure "expected constructor 11"

baselineV1MarketInfo :: MarketInfoV1.MarketInfoDatum
baselineV1MarketInfo =
    case Datum.fromBuiltinDataHex @MarketInfoV1.MarketInfoDatum marketInfoHex of
        Left err -> error err
        Right datum -> datum
  where
    marketInfoHex =
        "d8799fd8799f58201348ace5f4c75b56140c368d674d484291fff3c7604b06d96ed80501ed693fbb00ffd8799f581c91206651a11c64ad878968150d8b2d8b970dff46208c66b42a9df90a4c534e454b5f504552505f4c50ff1903e8530014df1050542d534e454b5f504552505f4c50530014df1059542d534e454b5f504552505f4c501b0000019c8c0a8aa01b00000001cf7c5800192710581c399b00074539d5c39c9d622e9afcfd1756e92f4ade0910df57a8bd39581cc3e0b44426e524ce86b5b1ae23c975aa7ae12bacf6b52dfd727c02d0581cfebe2c972dfbc6d4deb5a2a21409a2c5397fa440ff9307303d2d218a581c916d2114608f04cdf910f2e6655cf76da3c2e95d8a2bd8997dc32b35581cff0048fdba12c321bfc42d2ee9925d0d380ed26aa9f9bb2b0f30b2a1581c0c3b71e6468510341c813626cfa4d008d0b0c50d6ffcea188878e865d8799f581c14cfea925344e8f1c96985c8c1cf2e403e759b6d4113d52b1f996bf9530014df104c502d534e454b5f504552505f4c50ffd8799f581c95ff8558aeecf27e50871032069d250fce16561d820739bd6a7276f7581b535452494b455f4c505f534e454b5f334d5f30305f4f5241434c45ff1a004c4b4019cf3719373e1864194c2c18321901f4190bb8ff"

mkPaymentKeyHash :: String -> C.Hash C.PaymentKey
mkPaymentKeyHash hex = fromHexRawBytes (C.proxyToAsType $ Proxy @(C.Hash C.PaymentKey)) hex
